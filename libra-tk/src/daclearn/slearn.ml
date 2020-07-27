open Printf
module NSet = Circuit.NSet
module NMap = Circuit.NMap
open Circuit
open Ext
(*open Condpart*)
module F = Mn.Factor

type split = 
    {acf: Circuit.feature; 
     v: node;
     prob: float array array;
     vnodes: node list ; 
     (* mutable maxscore: float; *)
     mutable mine: int;
     mutable e: int option;
     mutable maxl: float; 
     mutable wt: float * float;
     mutable on_ev : bool;
     mutable final : bool;
     hashid: int}

(* This is greater-than instead of less-than so that we have a
   max-heap. *)
(* let split_lt d1 d2 = d1.maxscore > d2.maxscore *)
let split_eq s1 s2 = s1.hashid == s2.hashid

let split_hash split = split.hashid

module SplitHash = Hashtbl.Make(struct
                                type t = split
                                let equal = split_eq
                                let hash = split_hash
                              end)
module SplitHSet =
struct
  include SplitHash

  let add ns x = add ns x ()
  let iter f ns = iter (fun x () -> f x) ns
  let fold f ns = fold (fun x () l -> f x l) ns
  let to_list ns = fold (fun x l -> x :: l) ns []
  let sum_map f ns = fold (fun x accu -> f x + accu) ns 0
  let sumf_map f ns = fold (fun x accu -> f x +. accu) ns 0.0
  let filter f ns = iter (fun n -> if not (f n) then remove ns n) ns
end

let global_hashid = ref 0

let gen_hashid () =
  let id = !global_hashid in
  incr global_hashid ;
  assert (!global_hashid > 0) ;
  id

(*
 * Debugging
 *)

(* Print out a single candidate split. *)
let output_split out s =
  fprintf out "l=%f; e=%d; id=%d\n" s.maxl 
    (match s.e with None -> (-1) | Some n -> n) s.hashid

(*
 * Structure Learning
 *)

let collect_counts schema data =
  (* Collect all pairwise counts *)
  let numvars = Array.length schema in
  let marg_counts = Array.map (fun d -> Array.create d 0.0) schema in
  let create_counts i j = Array.create_matrix schema.(i) schema.(j) 0.0 in
  let joint_counts = 
    Array.init numvars (fun i -> Array.init i (create_counts i)) in
  let add_counts x=
    for i = 0 to numvars - 1 do
      let xi = x.(i) in
      marg_counts.(i).(xi) <- marg_counts.(i).(xi) +. 1.0;
      for j = 0 to i - 1 do
        let xj = x.(j) in
        joint_counts.(i).(j).(xi).(xj) <- joint_counts.(i).(j).(xi).(xj) +. 1.0;
      done;
    done in
  List.iter (add_counts) data;
  flush_all();
  (marg_counts, joint_counts, List.length data)

let compute_mi schema num_examples marg_counts joint_counts =
  (* Compute a single mutual information score *)
  let prior = 0.001 in
  let total = prior +. float_of_int num_examples in
  let calc_mi i j =
    let mi = ref 0.0 in
    let ip = prior /. (float_of_int schema.(i)) in
    let jp = prior /. (float_of_int schema.(j)) in
    let ijp = prior /. (float_of_int (schema.(i) * schema.(j))) in
    for ival = 0 to schema.(i) - 1 do
      for jval = 0 to schema.(j) - 1 do
        let p_ij = 
          (ijp +. joint_counts.(i).(j).(ival).(jval)) /. total in
        let p_i  = (ip +.  marg_counts.(i).(ival)) /. total in
        let p_j  = (jp +. marg_counts.(j).(jval)) /. total in
        if p_ij > 0. then
          mi := !mi +. p_ij *. log (p_ij /. (p_i *. p_j))
      done;
    done;
    !mi in

  (* Calculate all mutual informations *)
  let numvars = Array.length schema in
  let all_mi = Array.init numvars (fun i -> Array.init i (calc_mi i)) in
  all_mi


  


let rec add_edge all_mi visited edges chosen_edges i j mi ev_schema edge_tail minmu querynum =
  let numvars = Array.length visited in
  
  (*let selected_edges = ref selected_edges in 
  selected_edges := (i, j, mi):: !selected_edges;
  *)
  (* Add new edge *)
  
    
  let chosen_edges = (i, j, mi) :: chosen_edges in
  edge_tail.(i).(j) <- 1;
  edge_tail.(j).(i) <- 1;
  (* Add linked edges... *)
  let n = if visited.(i) then j else i in
  let get_mi n' =
    if n = n' then (n, n', 0.) 
    else begin
      let n1 = min n n' and n2 = max n n' in
      let mi = all_mi.(n2).(n1) in
      (n, n', mi) 
    end in
  let new_edges = Array.init numvars get_mi in
  Array.iter (Heap.add edges) new_edges;
  visited.(n) <- true;
  
  (* Get next best edge that won't create a cycle. *)

  let rec get_next () =
    if Heap.size edges == 0 then printf "Error! heap is empty.\n";
    let (i, j, mi) = Heap.min edges in
    (*printf "i: %d j: %d ev.(i):%d ev.(j):%d mi:%f\n" i j ev_schema.(i) ev_schema.(j) mi; 
    *)
   
    let n' = if i = n then j else i in
    Heap.remove_min edges;
    
    if (ev_schema.(i) >= 0  || ev_schema.(j) >= 0) then get_next() 
    else if visited.(i) && visited.(j) then get_next ()
    else (i,j, mi) in
   
   
   (*
    else if (ev_schema.(i) < 0 && ev_schema.(j) >= 0) || (ev_schema.(j) < 0 && ev_schema.(i) >= 0) then get_next()
    else if ev_schema.(i) >= 0 && ev_schema.(j) >= 0 then get_next()
    (*else if ev_schema.(i) < 0 && ev_schema.(j) < 0 then get_next()
    *) 
    else (
      if ev_schema.(i) >= 0 then (j,i, mi) else
      (i, j, mi) ) in
    *)
  
  
  (* Check to see if we're done.  (We'll end up with numvars-1 edges in
     the end, but for now, we have a dummy edge at the beginning.) *)
  if List.length chosen_edges >= (querynum) then
    chosen_edges
  else begin
    (* Select the best edge and recurse! *)
    let (i, j, mi) = get_next () in
    add_edge all_mi visited edges chosen_edges i j mi ev_schema edge_tail minmu querynum
  end

let add_more_edges all_mi min_mu numvars ev_schema edge_tail edge_num n edge_comp max_ev =
  let get_mi n' =
    if ev_schema.(n') < 0 then (n, n', 0.)
    else begin
      let n1 = min n n' and n2 = max n n' in
      let mi = all_mi.(n2).(n1) in
      (n', n, mi) 
    end in
  let new_edges = Array.init numvars get_mi in
  let edges = Heap.create edge_comp numvars in  
  Array.iter (Heap.add edges) new_edges;
  
  vlogf "query node %d: " n;
  let chosen_edges = ref [] in

  let min_edge = Heap.min edges in
  let (h,t,mu) = min_edge in
  chosen_edges := (Heap.min edges) :: !chosen_edges; vlogf "(%d, %0.3f) " h mu;
  Heap.remove_min edges;
  for k = 1 to max_ev - 1 do
    let min_edge = Heap.min edges in
    let (h,t,mu) = min_edge in
    if mu > min_mu then 
    (chosen_edges := (Heap.min edges) :: !chosen_edges; vlogf "(%d, %0.3f) " h mu;);
    Heap.remove_min edges;
  done;
  vlogf "\n";
  !chosen_edges
  



let chow_liu schema all_mi ev_schema minmu max_ev initedges=
  let numvars = Array.length schema in
  let querynum = Array.fold_left (fun s e -> if e < 0 then s + 1 else s) 0 ev_schema in
  let rec firstQueryVar var = 
    if ev_schema.(var) >= 0 then firstQueryVar (var+1)
    else var in

  let queryvar = firstQueryVar 0 in 
  vlogf "querynum : %d\n" querynum;
  let visited = Array.create numvars false in
  let edge_comp (_, _, mi1) (_, _, mi2) = mi1 > mi2 in
  let max_mu = ref (-1.0) in
  for i = 1 to numvars - 1 do
    for j = 0 to i - 1 do
      if i <> j && all_mi.(i).(j) > !max_mu then
        max_mu := all_mi.(i).(j);
    done;
  done;
  vlogf "max_mu: %f\n" !max_mu;
  let edges = Heap.create edge_comp 100 in
  let edge_tail = Array.make_matrix numvars numvars 0 in
  

  
  let all_edges = if initedges ="" then  add_edge all_mi visited edges [] queryvar queryvar 0.0 ev_schema edge_tail minmu  querynum 
    else begin
      let e = Data.load_data initedges in
      List.map (fun x->(x.(0)-1, x.(1)-1, 0.0) )  e end 
      in

  let ev_edges = ref [] in
  if numvars > querynum then begin
  for i = 0 to numvars - 1 do
    if ev_schema.(i) < 0 then begin
      let extra_edges =  add_more_edges all_mi (!max_mu *. minmu) numvars ev_schema edge_tail 0 i edge_comp max_ev in
      ev_edges := extra_edges @ !ev_edges;
    end;
  done;
  (*let new_edges = add_more_edges all_mi numvars ev_schema edge_tail ((numvars-1)*(numvars -2)/2) in 
   
  (* Remove the first dummy edge *)
  List.rev (new_edges @ List.tl (List.rev (all_edges)) ) 
  *)
  
  ( !ev_edges @ (List.tl (List.rev (all_edges))) )
  end
  else (List.tl (List.rev (all_edges)))
(* 
let output_cl_dot edges numvars ev_schema channel minmu = 
  fprintf channel "digraph g {\n";
  for i= 0 to numvars - 1 do
    if ev_schema.(i) >= 0 then
      fprintf channel "n%d [label = \"%d\", style=filled, color=red];\n" i i
    else
      fprintf channel "n%d [label = \"%d\", style=filled, color=green];\n" i i;
  done;
  Array.iter (fun (i,j,mi) ->  if mi >= minmu then  
        fprintf channel "\"n%d\" -> \"n%d\" [label=\"%f\"];\n" j i mi) edges;
  fprintf channel "}\n"
*)




let gen_splits fl c prior_stddev circ f data schema ev ev_schema extra_evs_schema ev_matched_data_list approx_percen split_cost edge_cost ev_logzs fseen =
  (* ENABLED *)
  (* For Boolean variables, only consider splitting on value=0,
     since splitting on value=1 is equivalent. *)
  
  let cl = List.map (fun f->conditions_to_ev schema (Array.of_list f.cond)) fl in 
  (*let fev = conditions_to_ev schema (Array.of_list f.cond) in
  let redun v = fev.(var_var v).(1 - (var_value v))<-log_zero; let res=  (List.exists (fun c-> ev_subset fev c) cl) in fev.(var_var v).(1 - (var_value v))<-log_one; res in
  fl  
  printf "create intial splits.\n";
  flush_all();ush_all(); *)
  let multival v = 
    not ((var_value v == 1) && circ.schema.(var_var v) == 2 ) in
  let mvars = List.filter multival (Array.to_list circ.flat_vnodes) in
  
  let vars = mvars in (* List.filter (fun v'-> not (feature_contains_var f v')) mvars in *)
  
  (*
  let vars = List.filter (redun) mvars in
  if (List.length vars) <> (List.length vars) then printf "FILTER redundant feature extension.\n";
  *)
  (*
  List.iter (fun v' ->let var = var_var v' in printf "%d, " var) vars;
  printf "\n";
  *)
  (* let vars = Array.to_list circ.flat_vnodes in *)
  
  
  (*let ev_logzs = Array.map (fun wev -> let (w,x,e) = wev in  
      let logz = compute_z_e circ e in
      (w,x,e,logz) ) ev  in 

  *)
  let (scores, data_counts, model_probs) = 
    OptImpl.score_feature_extensions fl c prior_stddev circ ev_logzs data f vars ev extra_evs_schema ev_matched_data_list approx_percen in
  
  let fprobs = Hashtbl.create 100 in
  List.iter2 (fun v p -> Hashtbl.add fprobs (var_details v) p) vars model_probs;
  
   (*for k= 1  downto 0 do
      let fcounts = Array.map (fun r->r.(k) ) data_counts in 
      feature_counts :=  fcounts :: !feature_counts;
      let f_support = ref 0.0 in
      for j = 0 to Array.length ev - 1 do
        let (w,_,_) = ev.(j) in
        f_support := !f_support +. w *. fcounts.(j);
      done;
      let s = !f_support in
      Heap.add feature_heap (List.nth f_new (dim - k), s);

    done;
  *)

  let scores = Array.of_list scores in
  (*printf "%f" (fst scores.(0)); *)
  let data_counts = Array.of_list data_counts in
  (* Generate splits *)
  (*flush_all();
  
   let (data_counts, fprobs) =  OptImpl.empirical_fx_probs3 circ f data vars ev ev_matched_data_list in
   let data_counts = Array.of_list data_counts in
    *)
  let mksplit i v = 
    (*let (fs,sd) = scores.(i) in 
    *)
    let vnodes = sibling_vars circ v in
      {acf=f; 
       prob=data_counts.(i); 


       v=v; 
       wt= (snd scores.(i));
       maxl= fst scores.(i); 
       (* maxscore=n *. (fst scores.(i)); *)
       e=None; 
       mine=4; (* HACK: This should be 2 if half_split is true. *)
       vnodes=vnodes; 
       on_ev = ev_schema.(var_var v) >= 0;
       final = false;
       hashid = gen_hashid ()} in
  
  (*printf "Selected vars: \n";
  List.iter (fun v -> printf "%d " (var_var v)) filtered_vars;
  printf "\n";
  *)
  let splits = List.mapi mksplit vars in

  (* Filter to remove redundant conditions *)
  let ev = conditions_to_ev circ.schema (Array.of_list f.cond) in
  let split_ok s =
    let (var, value) = var_details s.v in
    let ok1 = ref false and ok2 = ref false in
    for i = 0 to circ.schema.(var) - 1 do
      if ev.(var).(i) <> log_zero then
        if i = value then ok1 := true
        else ok2 := true
    done;
    !ok1 && !ok2 in
  let splits = List.filter split_ok splits in
  
  (* Exclude features with small gains *)
  let orig_num = List.length splits in
  let maxl = List.fold_left (fun v s -> max v s.maxl) 0.000001 splits in
  let splits = List.filter (fun s -> ((s.maxl >= maxl /. 100.) && (s.maxl > (split_cost +. 7.0 *. edge_cost)) ) ) splits in
  let new_num = List.length splits in
  vlogf "Pruned %d out of %d splits.\n" (orig_num - new_num) orig_num;
  List.iter (fun x-> NMap.add fseen x.acf.acnode fprobs) splits;
  splits

  
(* *** Initialization *** *)

let marginals schema data =
  (* Initialize marginal probabilities to empirical distribution,
       with one count of the uniform distribution added in to avoid
          zero probabilities (and thus weights of -inf). *)
  let u d = 1.0 /. (float_of_int d) in 
  let probs = Array.map (fun d -> Array.create d (u d)) schema in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    for j = 0 to Array.length data.(i) - 1 do
      probs.(j).(x.(j)) <- probs.(j).(x.(j)) +. 1.0
    done
  done;
  Array.iter normalize_inplace_raw probs;
  probs

(*let marginals schema data =
  (* Initialize marginal probabilities to empirical distribution,
       with one count of the uniform distribution added in to avoid
          zero probabilities (and thus weights of -inf). *)
  let u d = 1.0 /. float_of_int d in
  let probs = Array.map (fun d -> Array.create d (u d)) schema in
  let frac_count = 1. /. (float_of_int (Array.length data + 1)) in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    for j = 0 to Array.length data.(i) - 1 do
      probs.(j).(x.(j)) <- probs.(j).(x.(j)) +. frac_count
    done
  done;
  probs
*)
(*
let marginals schema data =
  let probs = Array.map (fun d -> Array.create d 0.) schema in
  let fraccount = 1. /. (float_of_int (Array.length data)) in
  for i = 0 to Array.length data - 1 do
    let x = data.(i) in
    for j = 0 to Array.length data.(i) - 1 do
      probs.(j).(x.(j)) <- probs.(j).(x.(j)) +. fraccount
    done
  done;
  probs
*)
let create_product_of_marginals schema probs =
  let fl = ref [] in
  let fp = ref [] in
  let r = create_times [] in
  let vnodes = make_vnodes schema in
  let vnodes_l = Array.map (Array.to_list) vnodes in
  for var = 0 to Array.length schema - 1 do 
      let p = create_plus [] in
   
      for i = 0 to schema.(var) - 1 do
        (* Add nodes for this value *)
        let logp = log probs.(var).(i) in
        let t = create_times [] in
        let theta = create_const logp in
        add_child t theta ;
        add_child t vnodes.(var).(i) ;
        add_child p t ;
        (* Construct feature *)
        let f = {acnode=theta;
                 cond = [(true, var, i)]; 
                 weight = logp;
                 ev=[||]} in  (* TODO: Set this? *)
        fl := f :: !fl;
        fp := probs.(var).(i) :: !fp
      done ;
      add_child r p;
  done ;
  let circ = of_graph schema vnodes vnodes_l r in
  (circ, !fl, !fp)


exception Done

let init c prior_stddev schema data ev ev_schema extra_evs_schema ev_matched_data_list split_cost minmu max_ev initedges =
  
 (* 
  let ev_vars= Array.mapi (fun i x -> if x >= 0 then i else -1) ev_schema in
  let q_vars = Array.mapi (fun i x -> if x < 0 then i else -1) ev_schema in
  
  let ev_vars = Array.of_list (List.filter (fun x-> x >= 0) (Array.to_list ev_vars)) in
  let q_vars = Array.of_list (List.filter (fun x-> x >= 0) (Array.to_list q_vars)) in
  
  let ev_data = Array.takeCol data ev_vars in
  
  *)
  (*for j = 0 to Array.length q_vars - 1 do
  let y = q_vars.(j) in
    printf "var: %d\n" y;
    let y_label = Array.map (fun d-> d.(y)) data in
    let wmat = create_weight_matrix 2 (Array.length ev_vars+1) in
  
    let (min, upoint)  = update_w wmat 100.0 0.1 ev_data y_label 2 (Array.length ev_vars) in
    Array.iter (printf "%f ") upoint.(0);
    printf "\n";
    Array.iter (printf "%f ") upoint.(1);
    printf "\n";

  done;
  *)
  (*
  Array.iter (fun q->Dtlearn.learn (Array.to_list data) ev_vars q) q_vars; 
  *)
  let probs = marginals schema data in
  let (circ, fl, fp) = create_product_of_marginals schema probs in

  (*let ds = Circuit.create_deriv_scratch circ in
  *)

(*
  let ev_logzs = Array.map (fun wev -> let (w,x,e) = wev in
      let nlogz = compute_z_e circ e in
      (w,x,e,nlogz) ) ev  in
  *)
  
  let numvars = (Array.length schema) in
  
  let (marg_counts, joint_counts, n) = collect_counts schema (Array.to_list data) in
  let mi = compute_mi schema n marg_counts joint_counts in
  let edges = Array.of_list (chow_liu schema mi ev_schema minmu max_ev initedges) in
  (*
  let ch = open_out "cl.dot" in
  output_cl_dot edges numvars ev_schema ch minmu;
  close_out ch;
  *)

  let degree = Array.make numvars 0 in
  Array.iter (fun (i,j,mi)->vlogf "%d->%d\n" i j; degree.(i)<- degree.(i) + 1; degree.(j)<- degree.(j) + 1;  ) edges;

  let multival v = 
    not ((var_value v == 1) && circ.schema.(var_var v) == 2 ) in
  
  let mvars = Array.of_list (List.filter multival (Array.to_list circ.flat_vnodes)) in
  let f_all_empir = Array.make (List.length fl) [||] in
  (* fl contains uni-variable features *)
  
  let splits = ref [] in
  let nf = float_of_int (Array.length data) in
  
  (*Array.iteri ( fun i d-> vlogf "node %d: %d " i degree.(i); if ev_schema.(i) >= 0 then printf "ev\n" else printf "\n";  ) degree;  
  *)
  let mmksplit i f vars mis varId =
    (*printf "mksplit i=%d f=%s\n" i (string_of_feature f);  
    *)
    (*let fprobs_l = OptImpl.expected_fx_probs circ f vars ev_logzs in *)
    let (extended_emp, f_empi) =  OptImpl.empirical_fx_probs3 circ f data vars ev ev_matched_data_list in
    (*fscore_2d2 c prior_stddev n evs extra_evs_schema f [||] emp model_probs vars *)
   
    f_all_empir.(i) <- f_empi;
    List.map3 ( fun v emp mi -> 
        let vnodes = sibling_vars circ v in
        let split =  {acf=f;
          prob= emp; 
          v=v; 
          wt=(0.0, 0.0);
          maxl= float_of_int(varId); (* (float_of_int ( numvars -  degree.(varId) )); (* + (if ev_schema.(var_var v) >= 0 then (- degree.(var_var v)) else degree.(var_var v) )  ) ) )); *) (* -. mi;*) *)
          (* maxscore=n *. (fst scores.(i)); *)
          e=None; 
          mine=4; (* HACK: This should be 2 if half_split is true. *)
          vnodes=vnodes; 
          on_ev = ev_schema.(var_var v) >= 0;
          final = false;
          hashid = gen_hashid ()
        } in (*let tailId = var_var v in degree.(varId) <- degree.(varId) - 1; degree.(tailId) <- degree.(tailId) - 1;*) split ) vars extended_emp mis
  in
   
  let f_var = Hashset.create numvars in
  vlogf "Create initial splits:\n";
  for k = 0 to (Array.length edges) - 1 do
      let (h,t,mi) = edges.(k) in
      vlogf "edge %d->%d\n" h t;  
      let rec nextFeature flist i va =
        (match flist with
          hf::tf -> let (s,v,va') = List.hd hf.cond in
                    if v = t && va' = va then (hf,i) else nextFeature tf (i+1) va) in
       
       (*let (ff,i) = nextFeature fl 0 1 in 
       *)
       let (ff,i) = if ev_schema.(h) < 0 then nextFeature fl 0 1 else nextFeature fl 0 0 in
       
       let split = mmksplit i ff [mvars.(h)] [0.0] k in
       splits := split @ !splits;
       
       (*let (ft,i) = nextFeature fl 0 1 in
       let split = mmksplit i ft [mvars.(h)] [0.0] k in
       splits := split @ !splits; *)
  done;
     
(*

  List.iteri (fun i f-> 
      let (s,v,va) = List.hd f.cond in
      let vars = ref [] in
      let mis = ref [] in
      (*if ev_schema.(v) < 0 then begin*)
      for k = 0 to (Array.length edges) - 1 do
        let (h,t,mi) = edges.(k) in
        if t = v then begin
          vars := mvars.(h) :: !vars;
          mis := mi :: !mis;
        end;
      done;
      if not (Hashset.mem f_var v) then (
        let split = mmksplit i f (List.rev !vars) (List.rev !mis) v in
        let split = List.filter (fun s-> s.maxl < (10.0 -. minmu )) split in 
        splits := split @ !splits;
        Hashset.add f_var v)
      (*end *)
      ) fl;
    
      *)
  List.iteri ( fun i f-> if (Array.length f_all_empir.(i)) = 0 then begin  
        let (extended_emp, f_empi) = OptImpl.empirical_fx_probs3 circ f data [] ev ev_matched_data_list in
        f_all_empir.(i) <- f_empi end ) fl; 
    
    (*let f_new = List.map (Slearn.split_feature !half_split circ !feature_list 
        anc !splits) in *)
  nlogf "Number of initial splits: %d\n" (List.length !splits); 

  (*let fsplits f = gen_splits c prior_stddev circ ev_logzs f data ev ev_schema extra_evs_schema in 
  let splits = List.flatten (List.map !fsplits fl) in
  *)
  
  let a = Array.transpose f_all_empir in
  (circ, fl, fp, !splits, Array.to_list (a) )

(* *** End Initialization *** *)

let lazy_ancl size anc nl =
  let key = List.hd nl in
  if not (NMap.mem anc key) then 
    NMap.add anc key (relatedl_a size parents nl) ;
  NMap.find anc key

let lazy_anc size anc n =
  if not (NMap.mem anc n) then 
    NMap.add anc n (relatedl_a size parents [n]) ;
  NMap.find anc n

let build_ancestors circ = 
  let h = NMap.create 100 in
  let hl = NMap.create 100 in
  let size = circ.size in
  function  
    | [x] -> let anc = lazy_anc  size h  x in a_mem anc
    |  l  -> let anc = lazy_ancl size hl l in a_mem anc


(* Generate a new AC feature by splitting the specified AC 
 * feature acf on the specified variable v *)
let gen_feature f sense v = 
  let (svar, svalue) = var_details v in
  let cond' = (sense, svar, svalue) :: f.cond in
  (* Multiply new parameter times old, since they are not
   * mutually exclusive features. *)
  let theta = create_const 0.0 in
  let f' = {acnode = theta; cond = cond'; weight = 0.0; ev=[||]} in
  f'


let split_dist_change ancl visited good_splits n n' =
  if is_times n then begin
    (* If we're an ancestor of just the feature or just the var for
     * any split, that means we may be among the set of nodes that
     * the split depends on.  Invalidate these splits.  
     *)
    let invalidate () =
      let process split =
        let danc = ancl [split.acf.acnode] n in
        let vanc = ancl split.vnodes n in
        if (danc && not vanc) || (vanc && not danc) then begin
          split.e <- None ;
          SplitHSet.remove good_splits split 
        end in
      SplitHSet.iter process good_splits in

    (* If we reduce the number of children or create
     * multiple copies of the node, then any edge estimate that
     * depends on this node could be invalid.
     *)
    if num_children n' < num_children n then 
      invalidate () 
    else if not (NSet.mem visited n) then 
      NSet.add visited n 
    else
      invalidate () 
  end


(*********************************************************
 * Splitting features and computing edge costs.
 * These are the main user interface for this module.
 *********************************************************)
 (* TODO: Document these parameters... *)

(* HACK: Declared as a global to reduce garbage collection *)
let good_splits = SplitHSet.create 100

exception StructureError 

let split_feature half_split circ features ancl all_splits split = 
  let f = split.acf and v = split.v in
  (*printf "f.id:%d, v.id:%d\n" (id f.acnode) (id v); *)
  let d_anc = ancl [f.acnode] in 
  let s_anc = ancl [v] in
  let vnodes = split.vnodes in 
  Hashes.clear_hashes () ;
  Hashes.clear_sets () ;
  let make_vhl v = (v, ancl [v], Hashes.get_hash ()) in
  let vhl = List.map make_vhl vnodes in

  let f' = gen_feature f true v in
  let g' = if half_split then f' else gen_feature f false v in
  let h_ma = Hashes.get_hash () in
  let h_f = Hashes.get_hash () in
  let h_t = Hashes.get_hash () in 
  
  let (s', var', value') = List.hd f.cond  in
  
  let parents' = Circuit.parents (List.nth circ.vnodes_l.(var') (if s' then value' else 1 - value')) in
  
  (*List.iter (fun n->printf "id: %d " (id n)) parents';
  printf "\n";
  *)

  if split.on_ev then begin
    vlogf "Extending circuit withiout duplicating\n";
    let ps = Circuit.parents f.acnode in
    
    (*if List.length ps <> 1 then raise StructureError;
    *)
    let (var, value) = var_details v in
    
    let s = create_plus [] in


    for i = 0 to circ.schema.(var) - 1 do
      let ind = circ.vnodes.(var).(i) in
      if i = value then begin
        let t = create_times [f'.acnode;ind] in
        add_child s t;
      end 
      else begin
        if half_split then add_child s ind
        else begin
          let t = create_times [g'.acnode; ind] in
          add_child s t;
        end
      end
    done;
    List.iter (fun p->add_child p s) ps;
  end else begin
    (* Replace orig. parameter node with a product with new feature's 
       parameter node *)
    let n' = create_node TimesNode [f'.acnode; f.acnode] in
    NMap.add h_t f.acnode n';
    if not half_split then
      (let m' = create_node TimesNode [g'.acnode; f.acnode] in
      NMap.add h_f f.acnode m');
    let visited = Hashes.get_set () in
    (* HACK: Declared as a global to reduce garbage collection *)
    SplitHSet.clear good_splits;
    List.iter (fun s -> if s.e <> None then 
                        SplitHSet.add good_splits s) all_splits ;

    let changed n n' = 
      if SplitHSet.length good_splits = 0 then ()
      else split_dist_change ancl visited good_splits n n' in
    let ncreate r children = create_node r.details children in
    (* let cl = remove_single_children details [] children in 
       create_node details cl in *)
    let r = Graphclone.copy_ma ncreate ignore ignore changed 
             (d_anc, s_anc, h_t, h_f, vhl, h_ma) circ.root in
    flush stdout;
    circ.root <- r;

  end;
  
  rebuild circ;
  f'.weight <- (fst split.wt);
  g'.weight <- (snd split.wt);
  set_feature_value f' (fst split.wt);
  set_feature_value g' (snd split.wt);

  if half_split then [f'] else [f'; g']

(* Alternate version -- less efficient, but always right. 
let delta_edges1 circ ancl max_edges split =
  let c = Circuit.copy circ in
  let root = c.root in
  let old_edges = List.sum (node_map num_children root) in
  let (_,_) = split_dist c ancl [] split [||] in
  let new_edges = List.sum (node_map num_children r) in
  new_edges - old_edges 
  *)

exception ManyEdges

let delta_edges half_split circ ancl max_edges split =

  Hashes.clear_hashes () ;
  Hashes.clear_sets () ; 
  let acf = split.acf and v = split.v in

  (* Find ancestors *)
  let d_anc = ancl [acf.acnode] in
  let s_anc = ancl [v] in
  let vnodes = split.vnodes in
  let v_anc = ancl vnodes in 

  (* Tricky hack to add edges every time the dist
     splitting code tries to create a ndoe. *)
  (* Initial number of edges is two, since adding a feature always 
   * adds a times node with an edge to the old param node and
   * an edge to a new param node. *)
  let edges = if half_split then ref 2 else ref 4 in
  let rem_edges e = 
    edges := !edges - e in
  let add_edges e = 
    edges := !edges + e; 
    if !edges > max_edges then raise ManyEdges in 

  (* Set of nodes we need to keep *)
  let marked  = Hashes.get_set () in 
  (* Set of nodes already removed *)
  let removed = Hashes.get_set () in
  let visit n =
    if not (NSet.mem marked n) && not (NSet.mem removed n) then 
      (* printf "Removing: "; print_endline n ; *)
      (rem_edges (num_children n) ;
       NSet.add removed n) in

  let rec mark_tree n =
    if not (NSet.mem marked n) && v_anc n then begin
      NSet.add marked n ;
      if NSet.mem removed n then 
        (* printf "Readding: "; print_endline n ; *)
        add_edges (num_children n) ;
      List.iter mark_tree n.children 
    end in

  let ncreate r children = 
    add_edges (List.length children) ; null_node in

  (* Allocate hashes *)
  let hashes = ref [] in
  let make_vhl v = 
    let h = Hashes.get_hash() in
    hashes := h :: !hashes;
    (v, ancl [v], h) in
  let vhl = List.map make_vhl vnodes in
  let h_ma = Hashes.get_hash () in
  let h_t = Hashes.get_hash () in 
  let h_f = Hashes.get_hash () in
  let changed n n' = () in
  let num_edges = 
    try 
      ignore(Graphclone.copy_ma ncreate mark_tree visit changed 
            (d_anc, s_anc, h_t, h_f, vhl, h_ma) circ.root);
      !edges 
    with ManyEdges -> max_edges + 1 in
  (* Return hashes and sets to the pool *)
  (*
  List.iter Hashes.release_hash [h_ma, h_t, h_f];
  List.iter Hashes.release_hash !hashes;
  List.iter Hashes.release_set [marked, removed]; *)
  num_edges
