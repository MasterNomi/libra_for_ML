open Str;;
open Ext;;
open Printf;;


(*let chi_critical = ref 3.841
*)


(*let feature_list = ref [];;
*)
let hashid = ref 0;;

let vars = ref (Hashtbl.create 100);;

let test_file = ref "";;

let val_file = ref "";;

let train_file = ref "";;

let model_file = ref "";;

let pruned_model_file = ref "";;

let pruned_node = ref 0

let use_gini = ref false

let do_prune = ref false

let var_num = ref 0
let chi_critical = ref 520.0
	
	(*@ common_arguments)*)


(*let _= train_file := Sys.argv.(1); val_file := Sys.argv.(2); test_file := Sys.argv.(3); model_file := Sys.argv.(4)
*)

type node = {
  id: int;
  mutable label: int; 
  mutable var: int; 
  mutable feature: string * bool;
  mutable left: int;
  mutable right: int; 
	mutable parent: int; 
}

let nodeArray = ref [||];; 
(*module NHashMap = Hashtbl.Make (struct
       type t = node
			 let equal a b = a.id == b.id
			 let hash a = a.id																																		   end)


let nodeSet = ref (NHashMap.create 100);;
*)
(*
let get_hashid() = let i = !hashid in incr !hashid;  i
*)

let create_node (f) = 
	let node = { id = !hashid; label = (-1); var = (-1); feature = f; left = 0; right = 0; parent = 0} in
	(*NHashMap.add !nodeSet node.id node;*)
	nodeArray := (Array.append !nodeArray [|node|]);
	incr hashid;
	(node)

let get_sample_label s classvar=
  s.(classvar)

	(*let l = List.nth s (!var_num) in l *)

let get_labels d classvar= List.map (fun s-> get_sample_label s classvar) d
	
let print_labels d classvar = 
	let labels = get_labels d classvar in
		List.iter (fun i-> print_int i; print_string " ") labels  
			

let log2 x = log x /. log 2.0

let entropy d classvar =
	let labels = get_labels d classvar in
	let sum = List.fold_left (fun i j -> i + j ) 0 labels in
	let size = List.length labels in
	let p  = (float_of_int sum /. float_of_int size) in
	if sum = size || sum = 0 then 0.0 
  else let ent = (p*.log2(p) +. (1.0 -.p)*.log2(1.0 -.p)) in 
	0.0-.ent


let info_gain set1 set2 classvar = 
	let set1_size = float_of_int(List.length set1) in
	let set2_size = float_of_int(List.length set2) in
	let total_size = set1_size +. set2_size in
		let set = List.concat [set1; set2] in 
			entropy set classvar -. (((set1_size /. total_size) *. entropy set1 classvar) +. ((set2_size /. total_size) *. entropy set2 classvar))


let gini_gain set1 set2 classvar=
  let get_p_i set = 
		let p_i = float_of_int(List.fold_left (fun p s -> let l = get_sample_label s classvar in p + l) 0 set) in
		let n_i = float_of_int (List.length set) -. p_i in
		(p_i, n_i) in
	let square x = x in 
	let set1_size = float_of_int(List.length set1) in
	let set2_size = float_of_int(List.length set2) in
	let (p1, n1) = get_p_i set1 in 
	let (p2, n2) = get_p_i set2 in
	let index = 1.0 -. (square(p1 /. set1_size)*.square(n1 /. set1_size)) -. (square(p2 /. set2_size)*.square(n2 /. set2_size)) in
		index
	 
	


let chi_square set1 set2 classvar = 
  let get_p_i set = 
		let p_i = float_of_int(List.fold_left (fun p s -> let l = get_sample_label s classvar in p + l) 0 set) in
		let n_i = float_of_int (List.length set) -. p_i in
		(p_i, n_i) in
	let (p1, n1) = get_p_i set1 in 
	let (p2, n2) = get_p_i set2 in
	let (p, n) = get_p_i (List.append set1 set2) in
	let get_' x pi ni =  x *. ((pi+.ni)/.(p+.n)) in
	let p'1 = get_' p p1 n1 in
	let p'2 = get_' p p2 n2 in
	let n'1 = get_' n p2 n2 in
	let n'2 = get_' n p2 n2 in
	(*Printf.printf "p1 %f n1 %f p2 %f n2 %f p'1 %2.2f n'1 %2.2f p'2 %2.2f n'2 %2.2f : " p1 n1 p2 n2 p'1 n'1 p'2 n'2;
	*)
	let square x = x *. x in 
		(square(p1 -. p'1) /. p'1) +. (square(p2 -. p'2) /. p'2) +. (square(n2 -. n'2) /. n'2) +. (square(n1 -. n'1) /. n'1)


(*let _= print_d (!dataset); print_labels !dataset
*)



(* TODO multidim *)
let split set v = 
	let satisfy sample v = 
		if sample.(v) = 0 then true else false in
	let (l1, l2) = List.partition (fun s -> satisfy s v ) set in
		(l1, l2)

let score_split split set v classvar = 
	let (l1,l2) = split set v in 
	(*let score = info_gain l1 l2 in*)
	if !use_gini then
		let score = gini_gain l1 l2 classvar in
			score
	else 
		let score = info_gain l1 l2 classvar in
	(*let s2 = chi_square l1 l2 in
	  (*print_float s2; print_string "\n";
		*)
		if s2 <= 6.635 then 0.0 else score*)
		score

exception Zero_Entropy

let find_best_var d varset classvar =
		let value_list = List.map (fun v -> score_split split d v classvar ) varset in
	let (var,value) = List.fold_left2 (fun m var value -> let (i,v) = m in if value >= v then (var,value) else (i,v)) ((-1),(0.0)) varset value_list in
		(var,value)

let num = ref 0;;

let print_features f = List.iter (fun i -> let (name,value) = i in if value then Printf.printf "!%s^" name else Printf.printf "%s^" name) f; print_string "\n"

let majority_label d classvar = 
		let p = List.fold_left (fun p s -> let l = get_sample_label s classvar in p + l) 0 d in
		let size = List.length d in
		let int_label = if (float_of_int p) > ((float_of_int size) /. 2.0 ) then 1 else 0 in
			(int_label)

let a = 10;;


let rec id3 d varset classvar features i parent left =
	let (var,value) = find_best_var d varset classvar in
	if var >= 0 then begin
		let (d1, d2) = List.partition (fun s -> s.(var) = 0) d in
		let chival =  chi_square d1 d2 classvar in
    (*printf "chival: %f\n" chival; *)
		(*if chival > !chi_critical then begin *)
    if i <= 4 then begin
			let new_varset = List.filter (fun v -> v <> var ) varset in
			let nf = (Hashtbl.find !vars var,false) in 
			let tf = (Hashtbl.find !vars var,true) in
			!nodeArray.(parent.id).var <- var;
			let nnode = create_node (nf) in
			!nodeArray.(nnode.id).parent <- parent.id;
			!nodeArray.(parent.id).left <- nnode.id;
			id3 d1 new_varset classvar (nf::features) (i+1) nnode true;
			let tnode = create_node (tf) in 
			!nodeArray.(tnode.id).parent <- parent.id;
			!nodeArray.(parent.id).right <- tnode.id;
			id3 d2 new_varset classvar (tf::features)(i+1) tnode false;

	end else begin
	  incr num;
		let int_label = majority_label d classvar in 
		!nodeArray.(parent.id).label <- int_label;
	end
	end else begin
	  incr num;
		let int_label = majority_label d classvar in 
		!nodeArray.(parent.id).label <- int_label;
	end


let rec print_tree node depth =
 (* print_string "in print \n";
	*)
	for i = 1 to depth do Printf.printf "| "; done;
	let (var, value) = node.feature in
		if depth >= 0 then begin
		Printf.printf "%s = %d : " var (if value then 1 else 0);
		
		if node.left = 0 && node.right = 0 then Printf.printf "%d" node.label;
		Printf.printf "\n";
		end;
	if node.left  > 0 then print_tree (!nodeArray.(node.left)) (depth + 1);
	if node.right > 0 then print_tree (!nodeArray.(node.right)) (depth + 1)



(*
let rec evaluate node sample = 
	let sample_ar = Array.of_list sample in
	if node.left = 0 && node.right = 0 then begin
		if node.label = get_sample_label sample then 1 else 0
	end else if  sample_ar.(node.var) = 0 then begin 
		let lnode = !nodeArray.(node.left) in
			evaluate lnode sample
	end else begin
		let rnode = !nodeArray.(node.right) in
			evaluate rnode sample
	end

*)
(*
let accuracy root d = 
		let res = List.map (evaluate root) d in
		let sum = List.fold_left ( fun s r -> let s = s + r in s ) 0 res in
		let size = List.length !testset in 
		let acc = (float_of_int sum) /. (float_of_int size) in 
			acc


*)
let isLeaf node = if (node.left = 0 && node.right = 0) then true else false


(*
let testprune node d set=

	let root = !nodeArray.(0) in 
	let old_acc = accuracy root d in
	let lnodeId = node.left in 
	let rnodeId = node.right in 
	!nodeArray.(node.id).left <- 0;
	!nodeArray.(node.id).right <- 0;

	!nodeArray.(node.id).label <- majority_label set;
  let new_acc = accuracy root d in

	
	if new_acc <= old_acc then begin
		!nodeArray.(node.id).left <- lnodeId;
		!nodeArray.(node.id).right <- rnodeId;
		false
	end else begin 
		incr pruned_node;
		(*Printf.printf "check node %d " node.id;
		Printf.printf " new_acc: %f old_acc: %f true\n" new_acc old_acc;
		*)
	true
	end



let rec prune node d set =
	if not (isLeaf node) then begin
		let (lset, rset) = List.partition (fun s-> let s_ar = Array.of_list s in s_ar.(node.var) = 0 ) set in
		if node.right <> 0 then
			let rnode = !nodeArray.(node.right) in
				prune rnode d rset;
		if node.left <> 0 then
			let lnode = !nodeArray.(node.left) in 
				prune lnode d lset
	end;
	if isLeaf node then begin
		(*Printf.printf "node %d is leaf\n" node.id;*)
		let parent = !nodeArray.(node.parent) in 
		if testprune parent d set then begin
			(*!nodeArray.(parent.id).label <- majority_label set;*)
			let root = !nodeArray.(0) in
				prune root d d
		end
 	end
*)

let learn data varlist classvar=
	vlogf "dtlearn.learn for %d: " classvar;
  nodeArray := [||];
  hashid := 0; 
 
  List.iter (fun v->Hashtbl.add !vars v (sprintf "X_%d" v)) varlist;   
  (*let root = create_node (("root", false)) in
	id3 data varlist classvar [] 0 root true; 
	*)
  
	let v_value = List.map (fun v -> (v,score_split split data v classvar) ) varlist in
  let sorted_v = List.sort ( fun (v,value) (v', value') -> if value > value' then -1 else if value < value' then 1 else 0) v_value in
  List.iter (fun (v, va) -> vlogf "(%d,%0.3f) " v va) sorted_v;
  vlogf "\n";


