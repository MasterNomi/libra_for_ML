open Extensions
open Printf

type sym_add_node = Leaf of Node.node
                  | Split of int * sym_add * sym_add
 and sym_add = int * sym_add_node

type sym_add_root = (int, unit) Hashtbl.t  * sym_add

(* Global numbering for nodes, so that they can be hashed *)
let zero_hashid = ref (-1)
let one_hashid = ref (-1)
let global_hashid = ref 0

let is_zero (id, x) = id = !zero_hashid 
let is_one (id, x) = id = !one_hashid
let is_nonzero (id, x) = id <> !zero_hashid 
let is_nonone (id, x) = id <> !one_hashid

let gen_hashid () =
  let id = !global_hashid in
  incr global_hashid ;
  assert (!global_hashid > 0) ;
  id

let make_split var left right =
  let id = gen_hashid () in
  (id, Split (var, left, right)) 


let create_split_node splithash var left right = 
  if left == right then 
    left 
  else 
    let key = (var, fst left, fst right) in
    if Hashtbl.mem splithash key then
      Hashtbl.find splithash key
    else
      let split = make_split var left right in
      (Hashtbl.add splithash key split ; split) 


let create_constleaf_node leafhash wt =
  if Hashtbl.mem leafhash wt then
    Hashtbl.find leafhash wt
  else
    let id = gen_hashid () in
    let l = Leaf (Node.create_and_print (Node.ConstNode wt) []) in
    if wt = neg_infinity then 
      zero_hashid := id
    else if wt = 0.0 then 
      one_hashid := id;
    let leaf = (id, l) in
    (Hashtbl.add leafhash wt leaf ; leaf)

let size (vars, root) =
  let h = Hashtbl.create 100 in
  let rec s (id_x, x) = 
    if (Hashtbl.mem h id_x) then 0
    else begin
      Hashtbl.add h id_x () ;
      match x with 
          Leaf n -> 1
        | Split (var, left, right) -> 1 + s left + s right
    end in
  s root


(* TODO... still need to fix this...
 * Generate an ADD for the indicator variables
 *)
let indicators_to_add vnodes v = 
  let create_leaf_node n = (gen_hashid (), Leaf n) in
  let left  = create_leaf_node vnodes.(v).(0) in
  let right = create_leaf_node vnodes.(v).(1) in
  Node.mini_output stdout vnodes.(v).(0) ;
  Node.mini_output stdout vnodes.(v).(1) ;
  let root = make_split v left right in 
  let cptvars = Array.create (Array.length vnodes + 1) false in
  cptvars.(v) <- true;
  (cptvars, root)


(* TODO
 * Generate an ADD for a CPT
 *)
let cpt_to_add s_hash l_hash varorder bn v =
  let cpt = bn.Bn.dists.(v) in
  (* cptvars.(i) is true if i'th var is part of this cpt *)
  let cptvars = Array.create (Bn.numvars bn + 1) false in
  Array.iter (fun p -> cptvars.(p.Bn.idx) <- true) cpt.Bn.parents ;
  cptvars.(v) <- true ;
  let state = Array.create (Bn.numvars bn) 0 in

  (* Recursively obtain all configurations of all variables *)
  let rec c2a = function 
    | x :: l when not cptvars.(x) -> 
        (* If this var is irrelevant, recurse. *)
        c2a l 
    | x :: l ->
        (* Recurse on each value of a relevant variable. *)
        let get_child i =
          state.(x) <- i;
          c2a l in
        let children = Array.init (Bn.get_range bn x) get_child in
        create_split_node s_hash x children
    | [] ->
        (* Base case: return leaf (AC node) for this configuration. *)
        let wt = Bn.cptvalue bn v state in
        if wt = neg_infinity then
          cptvars.(Array.length cptvars - 1) <- true;
        create_constleaf_node l_hash wt in

  let add = c2a (List.rev varorder) in
  (* print (cptvars, add) ; *)
  (cptvars, add)


let is_zero_leaf = function
    Split _ -> false
  | Leaf n -> Node.is_const n && (Node.const_value n = neg_infinity)

let is_one_leaf = function
    Split _ -> false
  | Leaf n -> Node.is_const n && (Node.const_value n = 0.0)

(* Variable ordering: Map ADD nodes to their rank in the list *)
let pos avarorder = function
    Leaf _ -> 100
  | Split (var, _, _) -> -avarorder.(var)


(* Optimized multiplication *)
let rec rec_mult avarorder cache (id_x, x) (id_y, y) = 

  (* Enforce ordering *)
  let (id_x, x, id_y, y) = 
    if pos avarorder x > pos avarorder y then 
      (id_y, y, id_x, x)
    else
      (id_x, x, id_y, y) in

  (* Check cache first *)
  if Hashtbl.mem cache (id_x, id_y) then 
    Hashtbl.find cache (id_x, id_y)

  else if id_x = !one_hashid || id_y = !zero_hashid then (id_y, y)
  else if id_y = !one_hashid || id_x = !zero_hashid then (id_x, x)

  (* Actual application... *)
  else 
    let result = 
      match x, y with
      (* Apply to two leaves (easy) *)
        Leaf n_x, Leaf n_y -> 
          Leaf (Node.create_and_print Node.TimesNode [n_x; n_y]) 

      (* When split variable is the same, recurse on the children *)
      | Split (x_var, x_l, x_r), Split (y_var, y_l, y_r) 
               when x_var == y_var -> 
          let var = x_var in
          let left  = rec_mult avarorder cache x_l y_l in
          let right = rec_mult avarorder cache x_r y_r in
          Split (var, left, right) 

      (* General case: recurse with each combo *)
      | Split (x_var, x_l, x_r), _ -> 
          let var = x_var in
          let left  = rec_mult avarorder cache x_l (id_y, y) in
          let right = rec_mult avarorder cache x_r (id_y, y) in
          Split (var, left, right) 

      (* The ordering should prevent the Leaf+Split case, always. *)
      | Leaf _, _ -> assert false in 
    let id = gen_hashid () in
    (Hashtbl.add cache (id_x, id_y) (id, result) ; (id, result)) 


let multiply avarorder (x_vars, x) (y_vars, y) =
  let cache = Hashtbl.create 100 in
  let product = rec_mult avarorder cache x y in
  let product_vars = Array.map2 ( || ) x_vars y_vars in
  (product_vars, product)


(*
 * Sum out a variable at the bottom of a SymADD
 *)

exception NonLeafException

let ac (id_x, x) = 
  match x with Leaf n -> n 
             | _ -> raise NonLeafException

let sum_out var (x_vars, (id_x, x)) =
  let x'_vars = Array.copy x_vars in
  x'_vars.(var) <- false;
  let hash = Hashtbl.create 100 in
  let rec so (id_y, y) =
    if Hashtbl.mem hash id_y then
      Hashtbl.find hash id_y
    else begin
      let ret = 
        match y with 
          Split (y_var, y_l, y_r) when y_var == var ->
            if is_zero y_l then y_r
            else if is_zero y_r then y_l
            else 
              (id_y, Leaf (Node.create_and_print Node.PlusNode [y_l; y_r]))
        | Split (y_var, y_l, y_r) ->
            (id_y, Split (y_var, so y_l, so y_r))
        | Leaf n -> 
            (id_y, Leaf n) in
      Hashtbl.add hash id_y ret ; ret
    end in
  (x'_vars, so (id_x, x))


exception EmptyList


(****************************
 * Minfill variable ordering
 ****************************)

let fill_edges adj elim v =
  let edge_list = ref [] in
  for i = 0 to Array.length elim - 1 do
    if not elim.(i) && adj.(v).(i) then
      for j = 0 to i - 1 do
        if not elim.(j) && adj.(v).(j) && not adj.(i).(j) then
          edge_list := (i,j) :: !edge_list 
      done
  done ;
  !edge_list

let fill_in adj elim v =
  let edges = fill_edges adj elim v in
  List.iter (fun (i,j) -> adj.(i).(j) <- true; adj.(j).(i) <- true) edges 

let minfill_var adj elim =
  let best_num = ref (max_int) in
  let best = ref (-1) in
  for i = 0 to Array.length elim - 1 do
    if not elim.(i) then
      let num_edges = List.length (fill_edges adj elim i) in
      if num_edges < !best_num then 
        (best := i ; best_num := num_edges)
  done ;
  !best


let minfill_next_var adj elim =
  let v = minfill_var adj elim in
  elim.(v) <- true ;
  fill_in adj elim v ;
  v


let minfill_order bn =

  (* Get clusters *)
  let numvars = Bn.numvars bn in 
  let clusters = 
    List.map (fun i -> i :: Array.to_list (Bn.parents bn i)) 
      (range (numvars-1)) in

  (* Construct adjacency table *)
  let adj = Array.create numvars [||] in
  for i = 0 to numvars - 1 do
    adj.(i) <- Array.create numvars false
  done ;

  (* Fill adjacency table *)
  let rec add_pairs = function
    | x::yl -> let add_y y = 
                 adj.(x).(y) <- true ;
                 adj.(y).(x) <- true in
               List.iter add_y yl ;
               add_pairs yl
    | [] -> () in
  List.iter add_pairs clusters ;

  let elim = Array.create numvars false in
  Array.to_list (Array.init numvars (fun _ -> minfill_next_var adj elim))

;;


(****************************
 * ADD Variable Elimination
 ****************************)


(* Efficient caching for ADD sizes 
 * Used for multiplying smallest ADDs first in varelim *)
let size_hash = Hashtbl.create 100 
let hsize (vars, root) =
  if Hashtbl.mem size_hash root then
    Hashtbl.find size_hash root
  else begin
    let s = size (vars, root) in
    Hashtbl.add size_hash root s ;
    s
  end 
let cmpsize a b = hsize a - hsize b 


let rec rec_eliminate avarorder addset = function
  | [] -> addset
  | v :: l ->
      (* For each variable in the elimination ordering, *)
      (* Select all ADDs with the variable *)
      start_time "elim";
      let has_v (vars,root) = vars.(v) in
      let (relevant, irrelevant) = List.partition has_v addset in

      (* Multiply smallest ones first  -- WORKS!!! *)
      let orelevant = List.sort cmpsize relevant in
      let product = List.fold_left (multiply avarorder) 
        (List.hd orelevant) (List.tl orelevant) in

      (* Pick the factor with fewest vars that aren't in current factor *
      let rec multall rel =
        match (List.sort cmpsize rel) with
          a :: [] -> a
        | a :: b :: l -> multall ((multiply avarorder a b) :: l)
        | [] -> raise EmptyList in
      let product = multall (indicator_adds.(v) :: relevant) in *)

      (* Always multiply smallest pair of factors next -- Similar
       * to previous method.
      let rec multall rel =
        match (List.sort cmpsize rel) with
          a :: [] -> a
        | a :: b :: l -> multall ((multiply avarorder a b) :: l)
        | [] -> raise EmptyList in
      let product = multall (indicator_adds.(v) :: relevant) in
       *)
      printf "Eliminating: %fs\n" (elapsed "elim") ;

      (* ...and sum out the eliminated variable *)
      let summed_out = sum_out v product in 
      printf "Summing out: %fs\n" (elapsed "elim") ;
      let addset' = summed_out :: irrelevant in
      rec_eliminate avarorder addset' l 


let eliminate addset varorder =
  let numvars = List.length varorder in
  let avarorder = Array.create numvars 0 in
  let rec build_order i = function
    | [] -> ()
    | x :: l -> avarorder.(x) <- i ; build_order (i+1) l in
  build_order 0 varorder ;
  rec_eliminate avarorder addset varorder


let varelim bn = 
  start_time "ve";
  let numvars = Bn.numvars bn in
  let schema = Bn.schema bn in
  let vn = Circuit.make_vnodes schema in
  (* let vnl = Array.map (Array.to_list) vn in *)

  (* Compute minfill variable ordering *)
  let varorder = minfill_order bn in
  printf "Minfill ordering: %fs\n" (elapsed "ve") ;

  (* Create ADDs for each variable *)
  let s_hash = Hashtbl.create 100 in
  let l_hash = Hashtbl.create 100 in
  let cpt_adds = Array.init numvars (cpt_to_add s_hash l_hash varorder bn) in
  let indicator_adds = Array.init numvars (indicators_to_add vn) in
  let all_adds = (Array.to_list cpt_adds) @ (Array.to_list indicator_adds) in
  printf "ADD creation: %fs\n" (elapsed "ve") ;
        
  (* Actual elimination *)
  let add_roots = eliminate all_adds varorder in

  (* Extract AC roots and multiply them together to get a single root *)
  let extract_ac (vars, (id_x, x)) = 
    match x with
      Leaf n -> n
    | _ -> raise NonLeafException  in
  let ac_roots = List.map extract_ac add_roots in
  match ac_roots with
     [r] -> r
   | _   -> Node.create_and_print Node.TimesNode ac_roots 

;;


(*************************
 * Simplification stuffs
 ************************

let get_leaf_value = function
  Leaf n -> exp (Node.const_value n)
| _ -> raise NonLeafException

let kld marg avarorder probs_and_nodes =
  let p_and_n = List.sort (fun (p,n) -> pos avarorder n) probs_and_nodes in
  let (probs, nodes) = List.split p_and_n in
  match l, r with 

  match l, r with
    Leaf a, Leaf b ->
      let lval = get_leaf_value a in
      let rval = get_leaf_value b in
      let qval = pl *. lval +. pr *. rval 
      (lval *. log lval + rval *. log rval) -. log qval
  | 
  *)
  
