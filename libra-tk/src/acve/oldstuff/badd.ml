open Extensions
open Printf

type sym_add_node = Leaf of int
                  | Split of int * sym_add array 
 and sym_add = int * sym_add_node

type sym_add_root = (int, unit) Hashtbl.t  * sym_add

(* Global numbering for nodes, so that they can be hashed *)
let global_hashid = ref 0

let gen_hashid () =
  let id = !global_hashid in
  incr global_hashid ;
  assert (!global_hashid > 0) ;
  id

(* Global numbering for leaves as well *)
let global_leafid = ref 0

let gen_leaf () =
  let id = !global_leafid in
  incr global_leafid ;
  assert (!global_leafid > 0) ;
  Leaf id

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

let create_leaf_node n = (gen_hashid (), Leaf n)

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
      printf "L %d\n";
  | Split (v, children) -> 
      printf "split on %d\n" v;
      Array.iter (print_offset (offset+1)) children in ()

let print (vars, root) = print_offset 0 root
;;


(* Variable ordering: Map ADD nodes to their rank in the list *)
let pos avarorder = function
    Leaf _ -> max_int
  | Split (var, _) -> -avarorder.(var)


(* Optimized multiplication *)
let rec rec_mult avarorder cache iol izl (id_x, x) (id_y, y) = 

  (* Enforce ordering *)
  if pos avarorder x > pos avarorder y then 
    rec_mult avarorder cache iol izl (id_y, y) (id_x, x)

  (* Check cache first *)
  else if Hashtbl.mem cache (id_x, id_y) then 
    Hashtbl.find cache (id_x, id_y) 

  (* iol = "is_one_leaf"; izl = "is_zero_leaf" *)
  else if iol x || izl y then (id_y, y)
  else if iol y || izl x then (id_x, x)

  (* Actual application... *)
  else 
    let result = 
      match x, y with
      (* Apply to two leaves (easy) *)
        Leaf n_x, Leaf n_y -> 
          (* SPEED DEBUG 
          Leaf (Node.create Node.TimesNode [n_x ; n_y]) *)
          Leaf (Node.create_const 0.5)

      (* When split variable is the same, recurse on the children *)
      | Split (x_var, x_children), Split (y_var, y_children) 
               when x_var == y_var -> 
          let var = x_var in
          let children = 
            Array.map2 (rec_mult avarorder cache iol izl) x_children y_children in
          Split (var, children) 

      (* General case: recurse with each combo *)
      | Split (x_var, x_children), _ -> 
          let var = x_var in
          let children = 
            Array.map (rec_mult avarorder cache iol izl (id_y, y)) x_children in
          Split (var, children) 

      (* The ordering should prevent the Leaf+Split case, always. *)
      | Leaf _, _ -> assert false in 
    let id = gen_hashid () in
    (Hashtbl.add cache (id_x, id_y) (id, result) ; (id, result)) 

let multiply avarorder (x_vars, x) (y_vars, y) =
  let cache = Hashtbl.create 100 in
  let product = rec_mult avarorder cache iol izl x y in
  let product_vars = Array.map2 ( || ) x_vars y_vars in
  (product_vars, product)


(*
 * Sum out a variable at the bottom of a SymADD
 *)

exception NonLeafException

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
                List.find (fun (id_c, c) -> is_one_leaf c) cl 
              with Not_found ->
                let children = List.map ac cl in
                (* SPEED DEBUG 
                (id_y, Leaf (Node.create Node.PlusNode children)) *)
                (id_y, Leaf (Node.create_const 0.5))
            end
        | Split (y_var, y_children) ->
            (id_y, Split (y_var, Array.map so y_children))
        | Leaf n -> 
            (id_y, Leaf n) in
      Hashtbl.add hash id_y ret ; ret
    end in
  (x'_vars, so (id_x, x))


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
  let vnl = Array.map (Array.to_list) vn in

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
  let multhash = Hashtbl.create 100 in

  let after = Sys.time() in
  printf "ADD creation: %fs\n" (after -. before) ;
  
  let rec eliminate addset = function
    | [] -> addset
    | v :: l ->
        (* DEBUG *)
        let total_size = List.sum_map size addset in
        printf "Total ADD size: %d\n" total_size ;
        let alt_size = Hashtbl.length s_hash + Hashtbl.length l_hash +
            Hashtbl.length multhash in
        printf "Alternate size: %d\n" alt_size ;
        flush stdout;

        (* For each variable in the elimination ordering, *)
        (* Select all ADDs with the variable *)
        let before = Sys.time() in
        let has_v (vars,root) = vars.(v) in
        let relevant = List.filter has_v addset in
        let irrelevant = List.filter (compose not has_v) addset in
        (* Multiply them... *)
        let product = List.fold_left 
            (multiply avarorder) indicator_adds.(v) relevant in
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
  let root = Node.create Node.TimesNode ac_roots in
  let before = Sys.time() in
  let c = Circuit.of_graph2 schema vn vnl root in
  let after = Sys.time() in
  printf "Creating circuit: %fs\n" (after -. before) ;
  c

;;

