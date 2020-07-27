open Extensions
open Printf

type sym_add_node = Leaf of Node.node
                  | Split of int * sym_add array 
 and sym_add = int * sym_add_node

type sym_add_root = (int, unit) Hashtbl.t  * sym_add

(* Global numbering for nodes, so that they can be hashed *)
let zero_hashid = ref (-1)
let one_hashid = ref (-1)
let global_hashid = ref 0

let gen_hashid () =
  let id = !global_hashid in
  incr global_hashid ;
  assert (!global_hashid > 0) ;
  id

let create_split_node splithash var children = 
  if Array.for_all ( (==) children.(0) ) children then
    children.(0)
  else 
    let lchildren = List.map fst (Array.to_list children) in
    if Hashtbl.mem splithash (var :: lchildren) then
      Hashtbl.find splithash (var :: lchildren)
    else
      let split = (gen_hashid (), Split (var, children)) in
      (Hashtbl.add splithash (var :: lchildren) split ; split) 


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
        | Split (var, children) -> 1 + Array.sum_map s children
    end in
  s root


(*
 * Print out a SymADD
 *)

let rec print_offset offset (id, x) =
  for i = 1 to offset do
    print_string "  "
  done ;
  printf "%d: " id;
  let _ = match x with 
    Leaf n -> 
      Node.print n ;
      print_string "\n"
  | Split (v, children) -> 
      printf "split on %d\n" v;
      Array.iter (print_offset (offset+1)) children in ()

let print (vars, root) = print_offset 0 root
;;


(*
 * Generate an ADD for the indicator variables
 *)
let indicators_to_add vnodes v = 
  let create_leaf_node n = (gen_hashid (), Leaf n) in
  let children = 
    Array.map create_leaf_node vnodes.(v) in
  Array.iter (Node.mini_output stdout) vnodes.(v) ;
  let root = (gen_hashid(), Split (v, children)) in
  let cptvars = Array.create (Array.length vnodes + 1) false in
  cptvars.(v) <- true;
  (cptvars, root)


(*
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
  | Split (var, _) -> -avarorder.(var)


(* Optimized multiplication *)
let rec rec_mult avarorder cache (id_x, x) (id_y, y) = 

  (* Enforce ordering *)
  if pos avarorder x > pos avarorder y then 
    rec_mult avarorder cache (id_y, y) (id_x, x)

  (* Check cache first *)
  else if Hashtbl.mem cache (id_x, id_y) then 
    Hashtbl.find cache (id_x, id_y) 

  else if id_x = !one_hashid || id_y = !zero_hashid then (id_y, y)
  else if id_y = !one_hashid || id_x = !zero_hashid then (id_x, x)

  (* Actual application... *)
  else 
    let result = 
      match x, y with
      (* Apply to two leaves (easy) *)
        Leaf n_x, Leaf n_y -> 
          Leaf (Node.create_and_print Node.TimesNode [n_x ; n_y]) 

      (* When split variable is the same, recurse on the children *)
      | Split (x_var, x_children), Split (y_var, y_children) 
               when x_var == y_var -> 
          let var = x_var in
          let children = 
            Array.map2 (rec_mult avarorder cache) x_children y_children in
          Split (var, children) 

      (* General case: recurse with each combo *)
      | Split (x_var, x_children), _ -> 
          let var = x_var in
          let children = 
            Array.map (rec_mult avarorder cache (id_y, y)) x_children in
          Split (var, children) 

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
exception NonLeafException_Debug

let ac (id_x, x) = 
  match x with Leaf n -> n 
             | Split _ -> raise NonLeafException
      
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
          Split (y_var, y_children) when y_var == var ->
            (* We don't need to add zeros *)
            let non_zero_leaf (id_c, c) = not (is_zero_leaf c) in
            let cl = List.filter non_zero_leaf (Array.to_list y_children) in
            (* If they were all zeros, then return the first one. *)
            if cl = [] then 
              y_children.(0)
            else if List.length cl = 1 then
              List.hd cl
            else begin
              try 
               (* Shortcut: if one of the children has a value of one,
                * then we're done.  (Assuming we had a BN to begin with.) *)
                List.find (fun (id_c, c) -> id_c = !one_hashid) cl 
              with Not_found ->
                let children = List.map ac cl in
                (id_y, Leaf (Node.create_and_print Node.PlusNode children)) 
            end
        | Split (y_var, y_children) ->
            (id_y, Split (y_var, Array.map so y_children))
        | Leaf n -> 
            (id_y, Leaf n) in
      Hashtbl.add hash id_y ret ; ret
    end in
  (x'_vars, so (id_x, x))


exception EmptyList


(* 
 * Optimized multiplication of everything at once! 
 *)
(* Each item in the list of roots is an (id, node) tuple *)
let rec rec_mult_all avarorder cache roots = 

  (* If there's a zero leaf, we're done *)
  let is_zero (id, x) = id = !zero_hashid in
  if List.exists is_zero roots then List.find is_zero roots
  else 
    let is_not_one (id, x) = id <> !one_hashid in
    let non1_roots = List.filter is_not_one roots in
    match non1_roots with
      [] -> List.hd roots (* They're all one, so return any of them. *)
    | r :: [] -> r        (* Only one left, so return it. *)
    | _ ->                (* ...general case... *)

  (* Sort first, to enforce ordering *)
  let cmp_roots (id_x, x) (id_y, y) =
    pos avarorder x - pos avarorder y in
  let roots = List.sort cmp_roots non1_roots in

  (* Check cache *)
  let tuple_id = List.map fst roots in
  if Hashtbl.mem cache tuple_id then 
    Hashtbl.find cache tuple_id
  else
  begin

    (* Actual application... *)
    let result = 
      match roots with

      (* Apply to leaves (easy).  If the first node is a leaf,
       * all others must be as well, due to ordering principles. *)
        (id, Leaf n) :: l ->
          let leaf_contents = function 
            | (id, Leaf n) -> n 
            | (id, Split _) -> raise NonLeafException_Debug in
          let leafdata = n :: List.map leaf_contents l in
          Leaf (Node.create_and_print Node.TimesNode leafdata)

      (* When split variable is the same, recurse on the children *)
      | (id, Split (var, children)) :: l ->
          let samevar = function 
            | (id, Split (v, cl)) -> v = var
            | _ -> false in
          let (varl, nonvarl) = List.partition samevar roots in
          let get_childrenl = function 
            | (id, Split (v, ca)) -> Array.to_list ca 
            | _ -> [] in
          let varl_children = List.transpose (List.map get_childrenl varl) in
          let recchildren = List.map (fun l -> l @ nonvarl) varl_children in
          let children = 
            List.map (rec_mult_all avarorder cache) recchildren in
          Split (var, Array.of_list children) 
       | [] -> raise EmptyList in

    let id = gen_hashid () in
    (Hashtbl.add cache tuple_id (id, result); (id, result)) 
  end

let multiply_all avarorder vars_and_roots =
  let cache = Hashtbl.create 100 in
  let (vars, roots) = List.split vars_and_roots in
  let product = rec_mult_all avarorder cache roots in
  let product_vars = Array.create (Array.length (List.hd vars)) false in
  for i = 0 to Array.length product_vars - 1 do
    product_vars.(i) <- List.fold_left (fun v a -> v || a.(i)) false vars
  done ;
  (product_vars, product)



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

let varelim bn = 
  let before = Sys.time() in

  let numvars = Bn.numvars bn in
  let schema = Bn.schema bn in
  let vn = Circuit.make_vnodes schema in
  (* let vnl = Array.map (Array.to_list) vn in *)

  (* Compute minfill variable ordering *)
  let varorder = minfill_order bn in
  let avarorder = Array.create numvars 0 in
  let rec build_order i = function
    | [] -> ()
    | x :: l -> avarorder.(x) <- i ; build_order (i+1) l in
  build_order 0 varorder ;

  let after = Sys.time() in
  printf "Minfill ordering: %fs\n" (after -. before) ;
  let before = Sys.time() in

  (* Create ADDs for each variable *)
  let s_hash = Hashtbl.create 100 in
  let l_hash = Hashtbl.create 100 in
  let indicator_adds = Array.init numvars (indicators_to_add vn) in
  let cpt_adds = Array.init numvars (cpt_to_add s_hash l_hash varorder bn) in
  (* let multhash = Hashtbl.create 100 in *)

  let after = Sys.time() in
  printf "ADD creation: %fs\n" (after -. before) ;

  (* Efficient caching for ADD sizes *)
  let size_hash = Hashtbl.create 100 in
  let hsize (vars, root) =
    if Hashtbl.mem size_hash root then
      Hashtbl.find size_hash root
    else begin
      let s = size (vars, root) in
      Hashtbl.add size_hash root s ;
      s
    end in
  let cmpsize a b = hsize a - hsize b in
  
  let rec eliminate addset = function
    | [] -> addset
    | v :: l ->
        (* DEBUG 
        let total_size = List.sum_map size addset in
        printf "Total ADD size: %d\n" total_size ;
        let alt_size = Hashtbl.length s_hash + Hashtbl.length l_hash +
            Hashtbl.length multhash in
        printf "Alternate size: %d\n" alt_size ;
        flush stdout; *)

        (* For each variable in the elimination ordering, *)
        (* Select all ADDs with the variable *)
        let before = Sys.time() in
        let has_v (vars,root) = vars.(v) in
        let (relevant, irrelevant) = List.partition has_v addset in

        (* Multiply smallest ones first  -- WORKS!!! *)
        let orelevant = List.sort cmpsize relevant in
        let product = List.fold_left (multiply avarorder) 
          indicator_adds.(v) orelevant in

        (* Always multiply smallest pair of factors next -- Similar
         * to previous method.
        let rec multall rel =
          match (List.sort cmpsize rel) with
            a :: [] -> a
          | a :: b :: l -> multall ((multiply avarorder a b) :: l)
          | [] -> raise EmptyList in
        let product = multall (indicator_adds.(v) :: relevant) in
         *)

        (* I tried multiplying indicators last. Doesn't work as well!
        let product = 
          List.fold_left (multiply avarorder) (List.hd relevant)
            (List.tl relevant) in
        let product = multiply avarorder product indicator_adds.(v) in
         *)
        (*
        let has_zero (vars,r) = vars.(Array.length(vars) - 1) in
        let (zero_rel, nonzero_rel) = List.partition has_zero relevant in
        (* Multiply them... *)
        let product = 
          if zero_rel = [] then
            multiply_all avarorder (indicator_adds.(v) :: relevant) 
          else
            List.fold_left (multiply avarorder) indicator_adds.(v) 
                (zero_rel @ nonzero_rel) in 
         *)
        let after = Sys.time() in
        printf "Eliminating: %fs\n" (after -. before) ;
        let before = Sys.time() in
        (* ...and sum out the eliminated variable *)
        let summed_out = sum_out v product in 
        let after = Sys.time() in
        printf "Summing out: %fs\n" (after -. before) ;
        let addset' = summed_out :: irrelevant in
        eliminate addset' l in
        
  let add_roots = eliminate (Array.to_list cpt_adds) varorder in
  let extract_ac (vars, (id_x, x)) = 
    match x with
      Leaf n -> n
    | Split _ -> raise NonLeafException in
  let ac_roots = List.map extract_ac add_roots in
  let root = Node.create_and_print Node.TimesNode ac_roots in
  root
  (* let before = Sys.time() in
  let c = Circuit.of_graph2 schema vn vnl root in
  let after = Sys.time() in
  printf "Creating circuit: %fs\n" (after -. before) ; 
  c *)

;;

