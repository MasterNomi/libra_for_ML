open Circuit
open Ext
open Printf
open Data

let pi = 4. *. atan 1.

(* log of a normal distribution with given stddev and zero mean *)
(* UNUSED, since the log(pi sigma^2)/2 term acts as an ad hoc
   per-split penalty of 0.572.  It's best to handle this explicitly.
let lognormal stddev x = 
  if stddev > 0. then
    -.(x *. x) /. (2. *. stddev) -. log (pi *. stddev) /. 2.
  else 0.
 *)

let lognormal stddev x = 
  if stddev > 0. then
    -.(x *. x) /. (2. *. stddev) 
  else 0.

let dlognormal stddev x =
  if stddev > 0. then
    -. x /. stddev
  else 0.

(* NOTE: See Circuit and Mn for type definitions of their
 * respective features. *)



let empirical_all_features_probs circ fa data evs ev_matched_data_list =
  (* Get counts *)
  (*let counts = Array.map (fun dim -> Array.create dim 0) circ.schema in
  *)
  (*let counts = Array.init (fun i->Array.init (Array.length evs) (fun j-> Array.make 2 0)) in 
  *)
  let all_probs = Array.init (Array.length evs) (fun j-> Array.make (Array.length fa) 0.0) in 
  
  (*let all_probs = Array.make (Array.length evs) 0.0 in*)
  
  
  for i = 0 to (Array.length fa) - 1 do
    let f = fa.(i) in
  let fcond = Array.of_list f.cond in

  
  (*let e_data = Array.to_list (Array.map (example_to_ev circ.schema) data) in
  *)

  let ev_matched_data_ar = Array.of_list ev_matched_data_list in
  for j = 0 to (Array.length evs) - 1 do
    let (w,x ,e) = evs.(j) in 
    
    (*let e = example_to_ev circ.schema x in *)
    
    let cond_ev = conditions_to_ev circ.schema fcond in

    
    if ev_intersect e cond_ev then begin
      let ev_matched_data = ev_matched_data_ar.(j) in
      (*let matched_data = List.filter (ev_intersect cond_ev) ev_matched_data in
      *)
      let ftotal = ref 0 in
      let add_counts x' =
        if ev_intersect x' cond_ev then incr ftotal;
        in
      
      List.iter add_counts ev_matched_data;
      if (List.length ev_matched_data) <> (int_of_float w) then begin 
          printf "you sucked :( \n"; 
          printf "w: %d ev_matched_data length:%d\n" (int_of_float w)  (List.length ev_matched_data); 
          exit 1; 
      end;  
      
      let total = float_of_int (List.length ev_matched_data) in
      
      (*let ftotal = Array.length matched_data in
      *)
      (*printf "ftotal :%d\n" !ftotal; *)
      let p_f = (float_of_int !ftotal) /. total in
      all_probs.(j).(i) <- p_f;
      (*
      let vnode_prob n =
        let (var, value) = var_details n in

        let p = (float_of_int counts.(var).(j).(value)) /. total in
        all_probs.(var).(j).(0) <- p;
        all_probs.(var).(j).(1) <- p_f -. p in 
      *)
      (*List.iter vnode_prob vars; *)

    end
  done;  
  done;
  (* Normalize into probabilities *)
  (Array.to_list all_probs)





let empirical_fx_probs2 circ fa data vars evs =
  printf "empirical_fx_probs2 vars: %d\n" (List.length vars);
  (* Get counts *)
  (*let counts = Array.map (fun dim -> Array.create dim 0) circ.schema in
  *)
  (*let counts = Array.init (fun i->Array.init (Array.length evs) (fun j-> Array.make 2 0)) in 
  *)
  let all_probs = Array.init (Array.length evs) (fun j-> Array.make (Array.length fa) 0.0) in 
  
  (*let all_probs = Array.make (Array.length evs) 0.0 in*)
  
  
  for i = 0 to (Array.length fa) - 1 do
    let f = fa.(i) in
  let fcond = Array.of_list f.cond in

  let e_data = Array.to_list (Array.map (example_to_ev circ.schema) data) in
  



  for j = 0 to (Array.length evs) - 1 do
    let (w,x) = evs.(j) in 
    let e = example_to_ev circ.schema x in
    
    let cond_ev = conditions_to_ev circ.schema fcond in


    
    if ev_intersect e cond_ev then begin
      
      let ev_and_cond = ev_union circ cond_ev e in  
      let ev_matched_data = List.filter (ev_intersect e) e_data in
      
      (*let matched_data = List.filter (ev_intersect cond_ev) ev_matched_data in
      *)
      let ftotal = ref 0 in
      let add_counts x' =
        if ev_intersect x' cond_ev then
          incr ftotal;
          (*for i = 0 to List.length vars - 1 do
            for k = 0 to circ.schema.(i) - 1 do
              if x'.(i).(k) >= 0.0 then counts.(i).(j).(k) <- counts.(i).(j).(k) + 1
            done
          done *) 
        in
      
      List.iter add_counts ev_matched_data;

      if (List.length ev_matched_data) <> (int_of_float w) then begin 
          printf "you sucked :( \n"; 
          printf "w: %d ev_matched_data length:%d\n" (int_of_float w)  (List.length ev_matched_data); 
          exit 1; 
      end;  
      
      let total = float_of_int (List.length ev_matched_data) in
      
      (*let ftotal = Array.length matched_data in
      *)
      printf "ftotal :%d\n" !ftotal;
      let p_f = (float_of_int !ftotal) /. total in
      all_probs.(j).(i) <- p_f;
      (*
      let vnode_prob n =
        let (var, value) = var_details n in

        let p = (float_of_int counts.(var).(j).(value)) /. total in
        all_probs.(var).(j).(0) <- p;
        all_probs.(var).(j).(1) <- p_f -. p in 
      *)
      (*List.iter vnode_prob vars; *)

    end
  done;  
  done;  
  printf "done\n"; 
  (* Normalize into probabilities *)
  (all_probs)


(*
let empirical_feature_probs circ fa data evs ev_matched_data_list =
  vlogf "empirical_fx_probs3 vars: %d\n" (List.length vars);
  (* Get counts *)
  (*let counts = Array.map (fun dim -> Array.create dim 0) circ.schema in
  *)
  let counts = Array.init (Array.length evs) (fun i->Array.make (Array.length fa) 0) in 
  let all_probs = Array.init (List.length evs) (fun i->Array.make (Array.length fa) 0.0) in 
  
  let ev_matched_data_ar= Array.of_list ev_matched_data_list in

  for j = 0 to (Array.length evs) - 1 do
    let (w,x,e) = evs.(j) in 
    (*let e = example_to_ev circ.schema x in *)
    let cond_ev = conditions_to_ev circ.schema fcond in
    
    if ev_intersect e cond_ev then begin
      let ev_and_cond = ev_union circ cond_ev e in  
      let ev_matched_data = ev_matched_data_ar.(j) in
      let ftotal = ref 0 in
      let add_counts x' =
        if ev_intersect x' cond_ev then
          incr ftotal;
        in
      
      List.iter add_counts ev_matched_data;

      if (List.length ev_matched_data) <> (int_of_float w) then begin 
          printf "w: %d ev_matched_data length:%d\n" (int_of_float w)  (List.length ev_matched_data); 
          exit 1; 
      end;  
      
      let total = float_of_int (List.length ev_matched_data) in
      
      (*let ftotal = Array.length matched_data in
      *)

      let p_f = (float_of_int !ftotal) /. total in
      let vnode_prob n =
        let (var, value) = var_details n in

        let p = (float_of_int counts.(var).(j).(value)) /. total in
        all_probs.(j) <- p_f;
        
        (*printf "p(f|e) : %f p(x|f,e): %f p(!x|f,e): %f x: %d f: %s\n" p_f all_probs.(var).(j).(0)  all_probs.(var).(j).(1) var (string_of_feature f);
        *)
      in
      List.iter vnode_prob vars; 

    end
  done;  
  (* Normalize into probabilities *)
  (Array.to_list all_probs)
*)



let empirical_fx_probs3 circ f data vars evs ev_matched_data_list =
  (*vlogf "empirical_fx_probs3 vars: %d f: %s\n" (List.length vars) (string_of_feature f); 
  flush_all();
  *)
  (* Get counts *)
  (*let counts = Array.map (fun dim -> Array.create dim 0) circ.schema in
  *)
  let counts = Array.init (List.length vars) (fun i->Array.init (Array.length evs) (fun j-> Array.make 2 0)) in 
  let all_probs = Array.init (List.length vars) (fun i->Array.init (Array.length evs) (fun j-> Array.make 2 0.0)) in 
  let f_probs = Array.make (Array.length evs) 0.0 in 
  
  let fcond = Array.of_list f.cond in
  let cond_ev = conditions_to_ev circ.schema fcond in


  let ev_matched_data_ar= Array.of_list ev_matched_data_list in

  for j = 0 to (Array.length evs) - 1 do
    let (w,x,e) = evs.(j) in 
    (*let e = example_to_ev circ.schema x in *)

     
    if ev_intersect e cond_ev then begin
      (*let ev_and_cond = ev_union circ cond_ev e in *) 
      let ev_matched_data = ev_matched_data_ar.(j) in
      let ftotal = ref 0 in
      (*printf "ev:\n";
      Array.iter (Array.iter (fun v->printf "%.0f " (exp v)) ) e;
      printf "\nfcond:\n";

      Array.iter (Array.iter (fun v->printf "%.0f "(exp v)) ) cond_ev;
      printf "\n"; 
      printf "data:\n";
      *)
      let add_counts x' =
        if ev_intersect x' cond_ev then begin
          (*Array.iter (Array.iter (fun v->printf "%.0f "(exp v)) ) x';
          printf "\n"; 
          *)
          incr ftotal;
          for i = 0 to List.length vars - 1 do
            let (var, value) = var_details (List.nth vars i)  in
            for k = 0 to circ.schema.(var) - 1 do
              (*TODO not working for multidimension*)
              if x'.(var).(k) = log_one && x'.(var).(1-k) = log_zero then counts.(i).(j).(k) <- counts.(i).(j).(k) + 1
            done
          done  
        end
        in
      
      List.iter add_counts ev_matched_data;
      if (List.length ev_matched_data) <> (int_of_float w) then begin 
          printf "w: %d ev_matched_data length:%d\n" (int_of_float w)  (List.length ev_matched_data); 
          exit 1; 
      end;  
      
      let total = float_of_int (List.length ev_matched_data) in
      
      (*let ftotal = Array.length matched_data in
      *)

      let p_f = (float_of_int !ftotal) /. total in
      f_probs.(j) <- p_f;
      let vnode_prob i n =
        let (var, value) = var_details n in


        (*TODO for multidimension. *)
        let ptotal = ref 0.0 in
        for k = 0 to circ.schema.(var) - 2 do
          let p = (float_of_int counts.(i).(j).(k)) /. total in
          (*printf "p(f|e)=%.20f p(f^x|e) = %f var = %d val=%d i = %d w=%f\n" p_f p var k i w; *)
          all_probs.(i).(j).(k) <- p; 
          ptotal := p +. !ptotal;
        done;
        (*
        printf "p(f|e)=%.20f p(f^x|e) = %f var = %d val=%d i = %d w=%f\n" p_f (p_f -. !ptotal) var (circ.schema.(var)-1) i w;
        *)
        
        (*let epsilon = 1.0e-10 in
        if (abs_float (!ptotal -. p_f)) > epsilon then 
        begin
          printf "ptotal:%f p_f:%f\n" !ptotal p_f; 
          printf "p_f is not equal to ptotal\n";
          exit 1;
        end;
        *)
        all_probs.(i).(j).(circ.schema.(var) - 1) <- p_f -. !ptotal;
        
        (*printf "p(f|e) : %f p(x|f,e): %f p(!x|f,e): %f x: %d f: %s\n" p_f all_probs.(var).(j).(0)  all_probs.(var).(j).(1) var (string_of_feature f);
        *)
      in
      List.iteri vnode_prob vars; 

    end;

  done;  
  (*printf "done\n"; *)
  (* Normalize into probabilities *)
  (Array.to_list all_probs, f_probs)





(* Callback for L-BFGS to get standard inclusive K-L divergence and gradient
 * according to an empirical distribution (samples).
 * Runs inference to get all probabilities.
 *)
let datall_val_and_grad ds prior_stddev circ true_fprobs n evs fa x g =
  (*Array.iter (fun (f, cond_ev)-> printf "f: %s\n" (string_of_feature f) )fa; 
  *)
  let begtime = Sys.time() in
  (* *** Set weights *** *)
  let fweights = x in 
  assert(Array.length fweights = Array.length g);
  Array.iter2 (fun (f,cond_ev) w-> set_feature_value f w) fa x; 

  (* *** Get derivatives *** *)
  (* let ze = conditions_to_ev circ.schema [] in *)
  let finalfx = ref 0.0 in
  for i = 0 to Array.length fa - 1 do
      g.(i) <- 0.0; 
  done ;
  let m = Array.length evs in

  let (w,ev,e) = evs.(0) in 
  
  (*let ncirc = sum_out_query_variables circ ev in
  printf "sum_out newsize:%d oldsize:%d" ncirc.size circ.size;
  *)

  for j = 0 to m - 1 do
    let (w,ev, e) = evs.(j) in 

    let fx = Array.sumf (Array.map2 ( *. ) true_fprobs.(j) fweights)  in
   (*
    let e = example_to_ev circ.schema ev.(j) in 
    let ds = create_deriv_scratch circ in
    get_derivatives_raw circ ds [||];
    let logz = ds.vr.(0) in 
    *)
    (*let logz = dss.(j).vr.(0) in *)
    
    (*let ev_logz = ev_logzs.(j) in *)
    
    (** TODO do this in advance *) 
    (*let e = example_to_ev circ.schema ev in*)
     
    (*let new_ev  = Array.of_list (List.filter (fun x-> x < 0 ) (Array.to_list ev)) in
    let new_e = example_to_ev ncirc.schema new_ev in
    let (ev_values, ev_cid) =  create_scratch ncirc in *)

    (*
    set_evidence ev_values ncirc new_e;
    let ev_cs = (ev_values, ev_cid) in 
    let ev_values = logvalues ev_cs ncirc in
    let ev_logz = ev_values.(0) in
     *)
 
   


    get_derivatives_raw circ ds e;
    
    let ev_logz = ds.vr.(0) in 

    let get_prob i (f,cond_ev) =
      
      if log_exists log_debug then begin
        Circuit.output_feature (log_stream log_debug) f;
        Circuit.output_node (log_stream log_debug) (feature_node f);
        dlogf "\ni = %d; id = %d; wt = %f\n" i (feature_node f).id fweights.(i)
      end;
      


      (*let e = example_to_ev circ.schema ev in *)
      
      (*
      let fcond = Array.of_list f.cond in
      let cond_ev = conditions_to_ev circ.schema fcond in
      *)



      let fprobi = ref 0.0 in
      if ev_subset cond_ev e then begin fprobi := 1.0 end
      else fprobi := exp(ds.dr.((feature_node f).id) +. fweights.(i) -. ev_logz);
      
      if !fprobi > 1.0 then begin
        if !fprobi > 1.000001 then (
          printf "FATAL fprob %f greater than 1.0 for %s\n" !fprobi (string_of_feature f);
          exit 1);
        fprobi := 1.0;
        (*
        if ev_subset cond_ev e then begin printf "is subset\n" end;
        let e = example_to_ev circ.schema ev in
        let ev_and_cond = ev_union circ cond_ev e in 
        let (ev_values, ev_cid) =  create_scratch circ in
        set_evidence ev_values circ ev_and_cond;
        let ev_cs = (ev_values, ev_cid) in 
        let ev_values = logvalues ev_cs circ in

        let pr_f_e = exp (ev_values.(0) -. ev_logz) in
        let pr_f_e2 = exp (ds.dr.((feature_node f).id) +. fweights.(i) -. ev_logz ) in 
        printf "fprob %f ds.dr=%f e(fw)=%f z=%f ev_z=%f\n" !fprobi (exp (ds.dr.((feature_node f).id))) (exp fweights.(i)) (exp logz) (exp ev_logz); 

        

        Array.iter (fun x-> Array.iter (fun b->printf "%0.0f " (exp b)) x; printf ", " ) ne; 
        printf "\n";
        Array.iter (fun x-> Array.iter (fun b->printf "%0.0f " (exp b)) x; printf ", " ) e; 
        printf "\n";
        Array.iter (fun x-> Array.iter (fun b->printf "%0.0f " (exp b)) x; printf ", " ) cond_ev; 
        printf "\n";
        Array.iter (fun x-> Array.iter (fun b->printf "%0.0f " (exp b)) x; printf ", " ) ev_and_cond; 
        printf "\n";


           
        let nds = create_deriv_scratch circ in
        get_derivatives_raw circ nds (conditions_to_ev circ.schema [||]);
        let pr_f = (exp (nds.dr.((feature_node f).id) +.  fweights.(i) -. nds.vr.(0))) in
        let (f_values, f_cid) =  create_scratch circ in
        set_evidence f_values circ cond_ev;
        let f_cs = (f_values, f_cid) in 
        let f_values = logvalues f_cs circ in
        let pr_f2 = exp (f_values.(0) -.  nds.vr.(0))  in
        
        let cond2 = [|(true, 13, 1)|] in
        let cond_ev2 = conditions_to_ev circ.schema cond2 in
        let ev_and_cond2 = ev_union circ cond_ev2 e in 

        let (ev_values2, ev_cid2) =  create_scratch circ in
        set_evidence ev_values2 circ ev_and_cond2;
        
        Array.iter (fun x-> Array.iter (fun b->printf "%0.0f " (exp b)) x; printf ", " ) ev_and_cond2; 
        printf "\n";
        let ev_cs2 = (ev_values2, ev_cid2) in
      
        let ev_values2 = logvalues ev_cs2 circ in
        printf "pr(not f, e) = %f\n" (exp (ev_values2.(0) -. nds.vr.(0)));
        
        printf "npr(e) = %f\n" (exp (nlogz -. nds.vr.(0))); 
        printf "pr_f= %f, pr_f2=%f\n" pr_f pr_f2;
       let (e_values, e_cid) =  create_scratch circ in
       set_evidence e_values circ ne;
       let e_cs = (e_values, e_cid) in
       let e_values = logvalues e_cs circ in
       printf "pr2(e) = %f\n" (exp (e_values.(0) -. nds.vr.(0))); 

        printf "pr(f,e) = %f\n" (exp (ev_values.(0) -. nds.vr.(0)));
        printf "pr(e) = %f\n" (exp (logz -. nds.vr.(0)));
        printf "pr3(e) = %f\n" (exp (ev_logz -. nds.vr.(0)));
        if pr_f_e2 > pr_f_e || pr_f_e > pr_f_e2 then printf "================================================\n"; 
        printf "pr(f|e) = %f pr2(f|e)=%f\n" pr_f_e2  pr_f_e;
        if pr_f_e2 > pr_f_e || pr_f_e > pr_f_e2 then printf "================================================\n"; 

        *)
        end;
       
      
      (*
      nlogf "i = %d; id = %d; wt = %f p = %f l = %d\n" i (feature_node f).id fweights.(i) !fprobi (Array.length true_fprobs);
      *)
      g.(i) <- g.(i) -. w *. (true_fprobs.(j).(i) -. !fprobi );
      !fprobi in 

    let fprobs = Array.mapi get_prob fa in
    (*if log_exists log_debug then
      for i = 0 to Array.length fa - 1 do
        dlogf "%d. Id: %d  Weight: %f  Expected: %f  True: %f\n" 
        i (feature_node fa.(i)).id fweights.(i) fprobs.(i) true_fprobs.(j).(i)
      done; *)

       
      (* *** Compute (negative) gradient *** *)
     
      
    (*printf "j,fx = %d, %f\n" j fx; *)
    finalfx := !finalfx +. w *. (fx -. ev_logz);  
  done;
 for i = 0 to Array.length fa - 1 do
        let dprior = dlognormal prior_stddev fweights.(i) in
        g.(i) <- g.(i)  +. (dprior /.  n)
      done ;
  let endtime = Sys.time() in
  
  let prior = Array.sumf_map (lognormal prior_stddev) fweights in  
  nlogf "LLg = %f (%f sec)\n" (!finalfx /. n) (endtime -. begtime);

  (-.(!finalfx) -. prior )



let empirical_fx_probs circ f data vars =
  vlogf "empirical_fx_probs\n";
  (* Get counts *)
  let counts = Array.map (fun dim -> Array.create dim 0) circ.schema in
  let fcond = Array.of_list f.cond in
  let ftotal = ref 0 in
  let add_counts x =
    if Mn.Factor.fmatch x fcond then 
      incr ftotal;
      for i = 0 to Array.length x - 1 do
        counts.(i).(x.(i)) <- counts.(i).(x.(i)) + 1
      done in
  Array.iter add_counts data;
  (* Normalize into probabilities *)
  let total = float_of_int (Array.length data) in
  let vnode_prob n =
    let (var, value) = var_details n in
    (float_of_int counts.(var).(value)) /. total in
  (*let ftotal = Array.count (fun x -> Mn.Factor.fmatch x fcond) data in
  *)
  let true_probs = List.map vnode_prob vars in
  let p_f = (float_of_int !ftotal) /. total in
  let all_probs = List.map (fun p -> (p, p_f -. p)) true_probs in
  all_probs



exception Converged
let datall_optimize_stochastic c prior_stddev circ true_fprobs n evs fa maxiter =
  vlogf "datall_optimize_stochastic\n";
  let ds = create_deriv_scratch circ in

  
  let x =  Array.map (fun f -> (feature_value f) ) fa in 
  let f_cond_ev_ar = Array.map (fun f-> let fcond = Array.of_list f.cond in
      let cond_ev = conditions_to_ev circ.schema fcond in
      (f, cond_ev) ) fa in
  let begtime = Sys.time() in
  let g = Array.make (Array.length fa) 0.0 in
  let indeces = Array.random (Array.length evs) in 

  let loss = ref 0.0 in
  let it = ref 0 in 
  try 


    while !it < maxiter do 
      incr it;
      
      let alpha = 1.0 /. (n ** sqrt(float_of_int(!it))) in 
      printf "it=%d loss=%f\n" !it !loss;
      Array.iter2 (fun (f,cond_ev) w-> set_feature_value f w) f_cond_ev_ar x; 
      let finalfx = ref 0.0 in
      let m = Array.length evs in
      let shuffled = Array.shuffle indeces in

      for i = 0 to Array.length fa - 1 do
        g.(i) <- 0.0; 
      done ;
      let (w,ev,e) = evs.(0) in 
      
      
      for l = 0 to m - 1 do
        let j = shuffled.(l) in 
        let (w,ev, e) = evs.(j) in    
        get_derivatives_raw circ ds e;
        let fx = Array.sumf (Array.map2 ( *. ) true_fprobs.(j) x)  in
       
        let ev_logz = ds.vr.(0) in 
        
        finalfx := !finalfx +. w *. (fx -. ev_logz);  
        
        let get_prob i (f,cond_ev) =
          let fprobi = ref 0.0 in
          if ev_subset cond_ev e then begin fprobi := 1.0 end
          else fprobi := exp(ds.dr.((feature_node f).id) +. x.(i) -. ev_logz);
          if !fprobi > 1.0 then begin
            printf "fprobi : %f is greater than one, Fatal error!\n" !fprobi ;
            fprobi := 1.0;
          end;
          let g =  w *. (true_fprobs.(j).(i) -. !fprobi ) in
          printf "j=%d g(%d):%f x(%d) :%f \n" j i g  i x.(i);
          let dprior = dlognormal prior_stddev x.(i) in
          x.(i) <- x.(i) +. alpha *. g -. alpha *. dprior;

          printf "after j=%d g(%d):%f x(%d) :%f \n" j i g  i x.(i) in
          Array.iteri get_prob f_cond_ev_ar;



      done;
      
      (*
      for i = 0 to Array.length fa - 1 do
        let dprior = dlognormal prior_stddev !x.(i) in
        g.(i) <- g.(i)  +. (dprior /.  n)
        x.(i) <- x.(i) -. alpha *. g.(i) -. alpha *. dprior;
      done ;
      *)
      
      let prior = Array.sumf_map (lognormal prior_stddev) x in 
      finalfx := !finalfx -. prior;

      let err = absf(!loss -. !finalfx) in
      printf "err = %f\n" err;
      if err < 0.00001 then raise Converged;
      loss := !finalfx;

    done;

    vlogf "Max iteration reached llg=%f\n" !loss;
    !loss
  with Converged-> begin vlogf "Converged after %d iteration. llg=%f\n" !it !loss; !loss end
  

(*
let data_ll prior_stddev circ ds true_fprobs fa n =
  let x = Array.map feature_value `fa in
  datall_val_and_grad prior_stddev circ ds true_fprobs n [||] fa x x
*)
let datall_optimize c prior_stddev circ true_counts n ev fa maxiter=

  let l = Array.length ev in
  (*
  let dss = Array.init  l (fun i->create_deriv_scratch circ) in
 
  let css = Array.init l (fun i->create_scratch circ) in
 
  let nev = Array.map (fun x -> let (w,e) = x in example_to_ev circ.schema e) ev in
  printf "here\n";
  let ev_logzs = Array.map2 (fun cs e-> let (ev_values, ev_cid) = cs in    
              set_evidence ev_values circ e;
              let ev_cs = (ev_values, ev_cid) in 
              let ev_values = logvalues ev_cs circ in
              ev_values.(0) ) css nev in


  printf "here2\n";
  
  let logz_and_ev = Array.map2 (fun ds e-> get_derivatives_raw circ ds e; (ds.vr.(0), e)) dss nev in
  let logzs = Array.map (fun ds->ds.vr.(0)) dss in
 *)
  
  let ds = create_deriv_scratch circ in
  let x = Array.map feature_value fa in 
  let f_cond_ev_ar = Array.map (fun f-> let fcond = Array.of_list f.cond in
      let cond_ev = conditions_to_ev circ.schema fcond in
      (f, cond_ev) ) fa in
      let llfunc = datall_val_and_grad ds prior_stddev circ true_counts n ev f_cond_ev_ar in
  let (errcode,ll) = Lbfgs.minimize_l1 c llfunc x 1.0e-3 maxiter in 
  nlogf "Errcode = %s; KLD = %f\n" (Lbfgs.errstring errcode) ll ;
  (-.ll)


let one_step_optimize c prior_stddev circ true_counts n ev fa alpha=

  let l = Array.length ev in
  let ds = create_deriv_scratch circ in

  let f_cond_ev_ar = Array.map (fun f-> let fcond = Array.of_list f.cond in
      let cond_ev = conditions_to_ev circ.schema fcond in
      (f, cond_ev) ) fa in
  let llfunc = datall_val_and_grad ds prior_stddev circ true_counts n ev f_cond_ev_ar in
  
  let grads = Array.make (Array.length fa) 0.0 in
  let origx = Array.map feature_value fa in 
  let x = Array.map (fun i->i) origx in

  let llg = llfunc x grads in
 
  Array.iteri (fun i f-> set_feature_value f (x.(i) -. alpha *. grads.(i)) ) fa;
  
  (*let (errcode,ll) = Lbfgs.minimize_l1 c llfunc x 1.0e-5 maxiter in 
  *)
  (-.llg)

let expected_fx_probs_v circ f v evs n =
  (* Compute derivatives, conditioned on feature *)
  
  vlogf "compute expectation for feature f:%s\n" (string_of_feature f); 
  (*printf "ev size : %d vars: %d\n" (Array.length evs) (List.length vars);
  *)
  let fcond = Array.of_list f.cond in
  let ncons = ref 0.0 in
  let cond_ev = conditions_to_ev circ.schema fcond in
  
  let fexample = Array.init (Array.length circ.schema) (fun i-> -1) in
  Array.iter (fun (sense, var, value) -> if sense then fexample.(var)<-value else fexample.(var)<- 1 - value) fcond;
  (*let all_probs = Array.make_matrix (List.length vars) 2 0.0 in 
  *)
  let all_probs = Array.init (Array.length evs) (fun j-> Array.make 2 0.0) in 

  let ds = create_deriv_scratch circ in 
  let nf = float_of_int n in
  let gamma =  1.0 /.   (float_of_int n) in
  let sn x = x.(0) in
  let fs x = x.(1) in
  let prior_stddev = 0.1 in
  for j = 0 to (Array.length evs) - 1 do
    let (w,x,e) = evs.(j) in 
    let ev_logz = compute_z_e circ e in
    (*printf "w = %0.0f, e=%s" w (to_string_example x);
    *)
    if ev_intersect e cond_ev then begin
      
      let ev_and_cond = ev_union circ cond_ev e in 
      
      get_derivatives_raw circ ds ev_and_cond;
    
      let ev_and_cond_logz = ds.vr.(0) in

      let fprob = if ev_subset cond_ev e then 1.0 else exp(ev_and_cond_logz -. ev_logz) in
      (*printf "p = %0.3f\n" p; *)
      
      
      
      
    
     (*Manually incorporate evidence on x *)
     
      for i = 0 to Array.length ev_and_cond - 1 do
        for k = 0 to Array.length ev_and_cond.(i) - 1 do
          if ev_and_cond.(i).(k) = log_zero then
            ds.dr.(circ.vnodes.(i).(k).id) <- log_zero
        done
      done; 
      

      (* Get probability P(f ^ x_i) for each variable x_i *)
      let node_to_prob n =   
        let (var', value') = var_details n in
        if x.(var') >= 0 && x.(var') = value' then fprob
        else if x.(var') >= 0 && x.(var') <> value' then 0.0 
        else if fexample.(var') >= 0 && fexample.(var') = value' then fprob 
        else if fexample.(var') >= 0 && fexample.(var') <> value' then 0.0
        else begin  
          let p = exp(ds.dr.(n.id) -. ev_logz ) in 
          if p > 1.0 then begin 
           
            printf "var:%d val:%d p:%f\n" var' value' p;
            printf "dr.dr.id = %f, ev_logz = %f exp ev_logz = %f\n" ds.dr.(n.id) ev_logz (exp ev_logz);
            printf "feature:  %s\n" (to_string_example fexample);
            printf "evidence: %s\n" (to_string_example x);
            1.0 
          end else
          p 

        end in 

      let f_x = node_to_prob v in

      (*printf "%s %s logz:%0.3f %0.3f\n" (to_string_example x) (string_of_feature f) (exp logz) fprob;
      *)
      
      
          let f_notx = fprob -. f_x in 
          if (f_x < 0.0) || (fprob < f_x )  then begin 
            printf "Error: P(f) < P(f^x)\n" ;
            printf "fprob:%0.9f (%0.10f %0.10f %0.10f),\n" (fprob) fprob f_x f_notx;
               
            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k) ) x; printf ", ") cond_ev;
            printf "\n";
            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") e;
            printf "\n";

            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") ev_and_cond;
            printf "\n";

            exit 1 
          end; 
          all_probs.(j).(0) <- f_x; all_probs.(j).(1) <- f_notx;
          
        

    end
    else begin
      ncons := !ncons +. w;
    end

   done;
   vlogf "%f evidence were inconsistent with the feature\n" !ncons;
   all_probs




(* *** *** *** *** *** *** *** *** *)

let expected_fx_probs_with_grads circ f vars ev_logzs empirical_probs n =
  (* Compute derivatives, conditioned on feature *)
  
  vlogf "compute expectation for feature f:%s\n" (string_of_feature f); 
  (*printf "ev size : %d vars: %d\n" (Array.length evs) (List.length vars);
  *)
  let fcond = Array.of_list f.cond in
  let ncons = ref 0.0 in
  let cond_ev = conditions_to_ev circ.schema fcond in
  
  let fexample = Array.init (List.length vars) (fun i-> -1) in
  Array.iter (fun (sense, var, value) -> if sense then fexample.(var)<-value else fexample.(var)<- 1 - value) fcond;
  (*let all_probs = Array.make_matrix (List.length vars) 2 0.0 in 
  *)
  let all_probs = Array.init (List.length vars) (fun i->Array.init (Array.length ev_logzs) (fun j-> Array.make 2 0.0)) in 
  let all_deltas = Array.init (List.length vars) (fun j-> Array.make 2 0.0) in 
  let scores =  Array.make (List.length vars) 0.0 in
  let all_scores =  Array.make (List.length vars) 0.0 in
  let empirical_probs_ar = Array.of_list(empirical_probs) in
  let ds = create_deriv_scratch circ in 
  let nf = float_of_int n in
  let gamma =  1.0 /.   (float_of_int n) in
  let sn x = x.(0) in
  let fs x = x.(1) in
  let prior_stddev = 0.1 in
  for j = 0 to (Array.length ev_logzs) - 1 do
    let (w,x,e,ev_logz) = ev_logzs.(j) in 
    
    (*printf "w = %0.0f, e=%s" w (to_string_example x);
    *)
    if ev_intersect e cond_ev then begin
      
     
     (*
      let (ev_values, ev_cid) =  create_scratch circ in
      set_evidence ev_values circ e;
      let ev_cs = (ev_values, ev_cid) in 
      let ev_values = logvalues ev_cs circ in
      
      let ev_logz = ev_values.(0) in
      *)
      
      let ev_and_cond = ev_union circ cond_ev e in 
      
      (*
      Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k) ) x; printf ", ") cond_ev;
      printf "\n";
      Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") e;
      printf "\n";

      Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") ev_and_cond;
      printf "\n";
      *)


      
      get_derivatives_raw circ ds ev_and_cond;
    
      let ev_and_cond_logz = ds.vr.(0) in

      (*if ev_logz <> ev_logz1 then begin printf "logz:%f logz1:%f\n" ev_logz ev_logz1; exit 1 end;
      *)

      (*let ev_and_cond_logz = compute_z_e circ ev_and_cond in
      *)
      (*
      let (ec_values, ec_cid) =  create_scratch circ in
      
      set_evidence ec_values circ ev_and_cond;
      let ec_cs = (ec_values, ec_cid) in 
      let ec_values = logvalues ec_cs circ in
      let p = exp(ec_values.(0) -. ev_logz) in
      *)

      let fprob = if ev_subset cond_ev e then 1.0 else exp(ev_and_cond_logz -. ev_logz) in
      (*printf "p = %0.3f\n" p; *)
      
      
      
      
    
     (*Manually incorporate evidence on x *)
     
      for i = 0 to Array.length ev_and_cond - 1 do
        for k = 0 to Array.length ev_and_cond.(i) - 1 do
          if ev_and_cond.(i).(k) = log_zero then
            ds.dr.(circ.vnodes.(i).(k).id) <- log_zero
        done
      done; 
      

      (* Get probability P(f ^ x_i) for each variable x_i *)
      let node_to_prob n =   
        let (var', value') = var_details n in
        if x.(var') >= 0 && x.(var') = value' then fprob
        else if x.(var') >= 0 && x.(var') <> value' then 0.0 
        else if fexample.(var') >= 0 && fexample.(var') = value' then fprob 
        else if fexample.(var') >= 0 && fexample.(var') <> value' then 0.0
        else begin  
          let p = exp(ds.dr.(n.id) -. ev_logz ) in 
          if p > 1.0 then begin 
           
            printf "var:%d val:%d p:%f\n" var' value' p;
            printf "dr.dr.id = %f, ev_logz = %f exp ev_logz = %f\n" ds.dr.(n.id) ev_logz (exp ev_logz);
            printf "feature:  %s\n" (to_string_example fexample);
            printf "evidence: %s\n" (to_string_example x);
            1.0 
          end else
          p 

        end in 

      let true_probs = List.map node_to_prob vars in

      (*printf "%s %s logz:%0.3f %0.3f\n" (to_string_example x) (string_of_feature f) (exp logz) fprob;
      *)
      
      List.iteri (fun i f_x->
          (*if absf ((log fprob) -. (log f_x)) < 0.0000001 then begin 
              all_probs.(i).(j).(0) <- fprob; 
              all_probs.(i).(j).(1) <- 0.0 
          end else if ((log f_x) < -1000.0) then begin
              all_probs.(i).(j).(1) <- fprob; 
              all_probs.(i).(j).(0) <- 0.0;
          end else *)
          let f_notx = fprob -. f_x in 
          if (f_x < 0.0) || (fprob < f_x )  then begin 
            printf "Error: P(f) < P(f^x) for i=%d\n" i;
            printf "fprob:%0.5f ( %0.10f %0.10f),\n" (fprob) f_x f_notx;
               
            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k) ) x; printf ", ") cond_ev;
            printf "\n";
            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") e;
            printf "\n";

            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") ev_and_cond;
            printf "\n";

            List.iter2 (fun v p->printf "v:%d p:%0.3f; " (var_var v) p) vars true_probs;
            printf "\n";
            exit 1 
          end; 
          all_probs.(i).(j).(0) <- f_x; all_probs.(i).(j).(1) <- f_notx
          

        ) true_probs;
      (*  

      let update_params  model empri delta i =
        let dprior_t = dlognormal prior_stddev delta.(0) in
        let dprior_f = dlognormal prior_stddev delta.(1) in
        let pf_data = (sn empri)  in
        let pt_data = (fs empri)  in
        let pt_w = model.(0) in
        let pf_w = model.(1) in
        let pt_w' = pt_w *. exp delta.(0)  in
        let pf_w' = pf_w *. exp delta.(1)   in
        let log_delta_z = log (pt_w' +. pf_w' +. 1. -. pt_w -. pf_w) in
        delta.(0) <-  delta.(0) -. gamma *. w  *. (pt_data -. (pt_w' /. (exp log_delta_z)))  -. gamma *. dprior_t;
        delta.(1) <-  delta.(1) -. gamma *. w  *. (pf_data -. (pf_w' /. (exp log_delta_z)))  -. gamma *. dprior_f; 
         
        (*let raw_value = delta.(0) *. pt_data +. delta.(1) *. pf_data -. log_delta_z in (*(pt_w' +. pf_w' -. log z') in *)
        ignore(raw_value)
        *)
        (*printf "j= %df = %s v: %d delta0: %f delta1: %f\n" j (string_of_feature f) (var_var (List.nth vars i)) delta.(0) delta.(1);
        *)
      in
      *)
      (*Array.iteri (fun i a->scores.(i)<- scores.(i) -. w*. update_params all_probs.(i).(j) empirical_probs_ar.(i).(j)  all_deltas.(i) i ) all_probs;
      *)
      (*Array.iteri (fun i a->update_params all_probs.(i).(j) empirical_probs_ar.(i).(j)  all_deltas.(i) i ) all_probs;
      *)
       (* Array.iteri (fun i a-> let (pt_data, pf_data) = true_probs_ar.(i) in all_grad.(i)<- all_grad.(i) +. absf(w *. (pt_data -. all_probs.(i).(j).(0))) +. absf( w *. (pf_data -. all_probs.(i).(j).(1)))) all_probs; *) 
      ignore();
    end
    else begin
      (* let update_params empri delta i =
        let dprior_t = dlognormal prior_stddev delta.(0) in
        let dprior_f = dlognormal prior_stddev delta.(1) in
        let pf_data = (sn empri)  in
        let pt_data = (fs empri)  in
        delta.(0) <-  delta.(0) -. gamma *. w  *. (pt_data) -. gamma *. dprior_t;
        delta.(1) <-  delta.(1) -. gamma *. w  *. (pf_data) -. gamma *. dprior_f;
      

        let raw_value = delta.(0) *. pt_data +. delta.(1) *. pf_data  in (*(pt_w' +. pf_w' -. log z') in *)
        ignore(raw_value)
    (*printf "j= %df = %s v: %d delta0: %f delta1: %f\n" j (string_of_feature f) (var_var (List.nth vars i)) delta.(0) delta.(1);
        *)
      in


      
      (*Array.iteri (fun i a->scores.(i)<- scores.(i) -. w*. update_params empirical_probs_ar.(i).(j) all_deltas.(i) i) all_deltas;
      *)
      (*Array.iteri (fun i a->update_params empirical_probs_ar.(i).(j) all_deltas.(i) i) all_deltas;
      *) *)
      ncons := !ncons +. w;
    end

   done;
    
  (*
  let get_score model empri delta i=

    if Circuit.feature_contains_var f (List.nth vars i) then (* || extra_evs_schema.(var_var v) >= 0 then *)  
      0.0
    else begin
    let pt_data = fs empri in
    let pf_data = sn empri in
    let pt_w = model.(0) in

    let pf_w = model.(1) in
    let pt_w' = pt_w *. exp delta.(0)  in
    let pf_w' = pf_w *. exp delta.(1)   in
    let log_delta_z = log (pt_w' +. pf_w' +. 1. -. pt_w -. pf_w) in
    let raw_value = delta.(0) *. pt_data +. delta.(1) *. pf_data -. log_delta_z in (*(pt_w' +. pf_w' -. log z') in *)
    raw_value 
    end
  in
  *)

  (*let prior delta = lognormal prior_stddev delta.(0)
          +. lognormal prior_stddev delta.(1) in
    *)
   (*for j = 0 to (Array.length ev_logzs) - 1 do
    let (w,x,e,ev_logz) = ev_logzs.(j) in 
    if ev_intersect e cond_ev then begin
      Array.iteri (fun i a->scores.(i)<- scores.(i) -. prior all_deltas.(i) -. w*. get_score all_probs.(i).(j) empirical_probs_ar.(i).(j) all_deltas.(i) i) all_probs;
    end
   done; 
    *)
   (*Array.iteri (fun i a->scores.(i)<- scores.(i) ) all_probs;
   *)
   (*Array.iteri (fun i delta->printf "f = %s v: %d delta0: %f delta1: %f\n" (string_of_feature f) (var_var (List.nth vars i)) delta.(0) delta.(1)) all_deltas;
    *)
  
  vlogf "%f evidence were inconsistent with the feature\n" !ncons;
  (* P(f ^ !x_i) = P(f) - P(f ^ x_i) *)
  (*let all_probs =Array.map (fun p -> let p_f = exp(p) /. n in let p_f_xi =  p_f -. (exp(!all_fprob) /. n)  in  printf "(%0.3f,%0.3f)" p_f p_f_xi; (p_f, p_f_xi )) all_true_prob in
  printf "\n";*)
  
  (*
   Array.iter (fun p -> Array.iter (fun y-> if y.(0) < 0.0 || y.(1) < -0.0 then begin printf "(%0.10f %0.10f)\n" y.(0) y.(1); exit 1; end) p) all_probs; 
  
  *)
  (Array.to_list all_probs) (*, (Array.to_list scores)) *)



let expected_fx_probs circ f vars ev_logzs=
  (* Compute derivatives, conditioned on feature *)
  
  vlogf "compute expectation for feature f:%s\n" (string_of_feature f); 
  (*printf "ev size : %d vars: %d\n" (Array.length evs) (List.length vars);
  *)
  let fcond = Array.of_list f.cond in
  let ncons = ref 0.0 in
  let cond_ev = conditions_to_ev circ.schema fcond in
  
  let fexample = Array.init (List.length vars) (fun i-> -1) in
  Array.iter (fun (sense, var, value) -> if sense then fexample.(var)<-value else fexample.(var)<- 1 - value) fcond;
  (*let all_probs = Array.make_matrix (List.length vars) 2 0.0 in 
  *)
  let all_probs = Array.init (List.length vars) (fun i->Array.init (Array.length ev_logzs) (fun j-> Array.make 2 0.0)) in 

  let ds = create_deriv_scratch circ in 
  for j = 0 to (Array.length ev_logzs) - 1 do
    let (w,x,e,ev_logz) = ev_logzs.(j) in 
    
    (*printf "w = %0.0f, e=%s" w (to_string_example x);
    *)
    if ev_intersect e cond_ev then begin
      
     
     (*
      let (ev_values, ev_cid) =  create_scratch circ in
      set_evidence ev_values circ e;
      let ev_cs = (ev_values, ev_cid) in 
      let ev_values = logvalues ev_cs circ in
      
      let ev_logz = ev_values.(0) in
      *)
      
      Timer.start "union";
      let ev_and_cond = ev_union circ cond_ev e in 
      Timer.stop "union";
      (*
      Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k) ) x; printf ", ") cond_ev;
      printf "\n";
      Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") e;
      printf "\n";

      Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") ev_and_cond;
      printf "\n";
      *)


      Timer.start "derivative"; 
      get_derivatives_raw circ ds ev_and_cond;
    
      Timer.stop "derivative";

      let ev_and_cond_logz = ds.vr.(0) in

      (*if ev_logz <> ev_logz1 then begin printf "logz:%f logz1:%f\n" ev_logz ev_logz1; exit 1 end;
      *)

      (*let ev_and_cond_logz = compute_z_e circ ev_and_cond in
      *)
      (*
      let (ec_values, ec_cid) =  create_scratch circ in
      
      set_evidence ec_values circ ev_and_cond;
      let ec_cs = (ec_values, ec_cid) in 
      let ec_values = logvalues ec_cs circ in
      let p = exp(ec_values.(0) -. ev_logz) in
      *)

      let fprob = if ev_subset cond_ev e then 1.0 else exp(ev_and_cond_logz -. ev_logz) in
      (*printf "p = %0.3f\n" p; *)
      
      
      
      
    
     (*Manually incorporate evidence on x *)
     
      for i = 0 to Array.length ev_and_cond - 1 do
        for k = 0 to Array.length ev_and_cond.(i) - 1 do
          if ev_and_cond.(i).(k) = log_zero then
            ds.dr.(circ.vnodes.(i).(k).id) <- log_zero
        done
      done; 
      

      (* Get probability P(f ^ x_i) for each variable x_i *)
      let node_to_prob n =   
        let (var', value') = var_details n in
        if x.(var') >= 0 && x.(var') = value' then fprob
        else if x.(var') >= 0 && x.(var') <> value' then 0.0 
        else if fexample.(var') >= 0 && fexample.(var') = value' then fprob 
        else if fexample.(var') >= 0 && fexample.(var') <> value' then 0.0
        else begin  
          let p = exp(ds.dr.(n.id) -. ev_logz ) in 
          if p > 1.0 then begin 
            if p > 1.000001 then ( 
            
            printf "var:%d val:%d p:%f\n" var' value' p;
            printf "dr.dr.id = %f, ev_logz = %f exp true-ev_logz = %f\n" ds.dr.(n.id) ev_logz (compute_z_e circ e);
            printf "ERROR: Incorrect probability.\n";

            exit 1);
            (*printf "feature:  %s\n" (to_string_example fexample);
            printf "evidence: %s\n" (to_string_example x);
            *)
            1.0 
          end else  
          p 

        end in 

      let true_probs = List.map node_to_prob vars in

      (*printf "%s %s logz:%0.3f %0.3f\n" (to_string_example x) (string_of_feature f) (exp logz) fprob;
      *)
      List.iteri (fun i f_x->
          let lfx = log f_x in
          if absf ((log fprob) -. (lfx)) < 0.0000001 then begin 
              all_probs.(i).(j).(0) <- fprob; 
              all_probs.(i).(j).(1) <- 0.0 
          end else if (lfx < -1000.0) then begin
              all_probs.(i).(j).(1) <- fprob; 
              all_probs.(i).(j).(0) <- 0.0;
          end else  
          let f_notx = fprob -. f_x in 
          if (f_x < 0.0) || (fprob < f_x )  then begin 
            printf "Error: P(f) < P(f^x) for i=%d\n" i;
            printf "fprob:%0.10f ( %0.10f %0.10f),\n" (fprob) f_x f_notx;
               
            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k) ) x; printf ", ") cond_ev;
            printf "\n";
            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") e;
            printf "\n";

            Array.iter (fun x->Array.iter (fun k->printf "%0.0f " (exp k)) x; printf ", ") ev_and_cond;
            printf "\n";

            List.iter2 (fun v p->printf "v:%d p:%0.3f; " (var_var v) p) vars true_probs;
            printf "\n";
            exit 1 
          end; 
          all_probs.(i).(j).(0) <- f_x; all_probs.(i).(j).(1) <- f_notx
        ) true_probs;
    end
    else begin
      ncons := !ncons +. w;
    end
  done;
  
  (*let n = float_of_int (Array.length evs) in
  *)
  vlogf "%f evidence were inconsistent with the feature\n" !ncons;
  (* P(f ^ !x_i) = P(f) - P(f ^ x_i) *)
  (*let all_probs =Array.map (fun p -> let p_f = exp(p) /. n in let p_f_xi =  p_f -. (exp(!all_fprob) /. n)  in  printf "(%0.3f,%0.3f)" p_f p_f_xi; (p_f, p_f_xi )) all_true_prob in
  printf "\n";*)
  
  (*
   Array.iter (fun p -> Array.iter (fun y-> if y.(0) < 0.0 || y.(1) < -0.0 then begin printf "(%0.10f %0.10f)\n" y.(0) y.(1); exit 1; end) p) all_probs; 
  
  *)
  (Array.to_list all_probs)


(*

let learn2d circ data ev vars f c  =  
  let data_probs = empirical_fx_probs circ f data vars in
    
  
  let func delta gradient = 
    (* Compute value *)
    let pt_w' = pt_w *. exp delta.(0) in
    let pf_w' = pf_w *. exp delta.(1) in
    let z' = pt_w' +. pf_w' +. 1. -. pt_w -. pf_w in
    let prior = lognormal prior_stddev delta.(0)
        +. lognormal prior_stddev delta.(1) in
    let raw_value = 
        delta.(0) *. pt_data +. delta.(1) *. pf_data -. log z' in
    let value = -.(raw_value *. n +. prior) -. gradient.(0) in

    (* Compute gradient *)
    
    let dprior_t = dlognormal prior_stddev delta.(0) in
    let dprior_f = dlognormal prior_stddev delta.(1) in
    let dft_dx = pt_data -. pt_w' /. z' in
    let dff_dx = pf_data -. pf_w' /. z' in
    gradient.(0) <- -.(n *. dft_dx +. dprior_t);
    gradient.(1) <- -.(n *. dff_dx +. dprior_f);
    value in

  ignore()



*)

let isnan x = not (x=x)

exception Prune_exception

let fscore_2d c prior_stddev n evs extra_evs_schema f true_probs model_probs v  =
  if Circuit.feature_contains_var f v then (* || extra_evs_schema.(var_var v) >= 0 then *)  
    (0.0, (0.0, 0.0))
  else begin 
  let (var, value) = var_details v in
  vlogf "fscore:  %s %d %d\n"  (string_of_feature f) var value; 
  (*print_string "l1: "; print_float c; print_string "\n";
  *)
  (*  
  let n_pt_d = ref 0.0 in 
  let n_pt_w = ref 0.0 in 
  let n_pf_d = ref 0.0 in 
  let n_pf_w = ref 0.0 in 
  *)
  let it = ref 1 in
  
  let (pt_data, pf_data) = true_probs in 
  printf "pt_data:%f, pf_data:%f\n" pt_data pf_data;
  let func  delta gradient = 
    (* Compute value *)
    let g = gradient.(0) in
    let value = ref 0.0 in
    let total_pt_data = ref 0.0 in
    let total_pf_data = ref 0.0 in
    let total_pt_w' = ref 0.0 in
    let total_pf_w' = ref 0.0 in 
    
    (*
    n_pt_d := 0.0;
    n_pt_w := 0.0;
    n_pf_w := 0.0;
    n_pf_d := 0.0;
  
    *)


    for j = 0 to (Array.length evs) - 1 do 
    

      let (w,e, e') =  evs.(j) in
      let pt_w = model_probs.(j).(0) and pf_w = model_probs.(j).(1) in
      let pt_w' = pt_w *. exp delta.(0)  in
      let pf_w' = pf_w *. exp delta.(1)   in

      let log_delta_z = log (pt_w' +. pf_w' +. 1. -. pt_w -. pf_w) in
      
      (*n_pt_w := !n_pt_w +. w *.  pt_w;
      n_pt_d := !n_pt_d +. w *.  pt_data;
      n_pf_w := !n_pf_w +. w *.  pf_w;
      n_pf_d := !n_pf_d +. w *.  pf_data;
      *)

      (*if isnan !value then  begin 
        printf "j=%d z'=%f pt_w:%f pf_w:%f t1:%0.3f t2:%0.3f\n" j log_delta_z pt_w pf_w delta.(0) delta.(1); 
      end;
      *)
      total_pt_w' :=  !total_pt_w' +. w *. (pt_w' /. (exp log_delta_z) );
      total_pf_w' :=  !total_pf_w' +. w *. (pf_w' /. (exp log_delta_z) );
      
      (*total_pt_data := !total_pt_data +. pt_data *. w;
      total_pf_data := !total_pf_data +. pf_data *. w;
      *)
      let raw_value = log_delta_z in (*(pt_w' +. pf_w' -. log z') in *)
      
      (*printf "rawval: %f\n" raw_value;
        *)  
       (*delta.(0) *. pt_data +. delta.(1) *. pf_data -. log z' in *)
      
      (*if isnan raw_value then begin
        printf "ERROR: raw value is nan\n";
        printf "pt_data:%0.5f pf_data:%0.5f delta0:%0.5f delta1:%0.5f log_delta_z:%0.5f\n" pt_data pf_data delta.(0) delta.(1) log_delta_z;
      end;*)
      value := !value +. w *. raw_value; 
      dlogf "i:%d in grad, j:%d value:%0.5f rawvalue:%0.5f delta0:%0.5f delta1:%0.5f w:%0.1f pt_w:%f pf_w:%f pt_d:%f pf_d:%f lz:%f\n" !it j !value raw_value delta.(0) delta.(1) w pt_w pf_w pt_data pf_data log_delta_z;
    done;

    let prior = lognormal prior_stddev delta.(0)
          +. lognormal prior_stddev delta.(1) in
   
   
    
    let dprior_t = dlognormal prior_stddev delta.(0) in
    let dprior_f = dlognormal prior_stddev delta.(1) in
    
    (*printf "ptdata:%f, pfdata:%f\n tpt:%f tpf:%f\n" pt_data pf_data !total_pt_w' !total_pf_w';
    *)
    let dft_dx = (pt_data) -. !total_pt_w' in
    let dff_dx = (pf_data) -. !total_pf_w' in
    gradient.(0) <- -.(dft_dx +. dprior_t);
    gradient.(1) <- -.(dff_dx +. dprior_f);
    let gradient_mag = absf(gradient.(0)) +. absf(gradient.(1))  in
    
    (*if !it = 1 && gradient_mag < 0.1 then raise Prune_exception;
     *)
    value:= !value -. delta.(0) *. pt_data -. delta.(1) *. pf_data -. prior; (* -. gradient.(0)  *);
    
    incr it;
    
    dlogf "grad.0=%0.6f grad.1=%0.6f value=%0.6f\n" gradient.(0) gradient.(1) !value;  
    (* Compute gradient *)
     (*printf "value in gradient:%0.5f g:%0.5f %0.5f prior:%0.3f\n" !value gradient.(0) gradient.(1) prior;
    *)
    
    let gradient_mag = absf(gradient.(0)) +. absf(gradient.(1))  in
    
    (*printf "gradient mag:%f\n" gradient_mag;
    *)
    !value 

    in

  let (w,ev, e) = evs.(0) in

  let nprior = ref 0.0  in 
  (*if ev.(var) >= 0 then nprior := 100.0; *) 
  (* Maximize log likelihood gain by minimizing its negative with LBFGS *)
  let len = Array.length evs in
  
  (*let random_set = Array.init (len) (fun i -> i)  in
  let myf = func random_set in 
  *)
  (*let random_set2 = Array.init (len/1) (fun i -> Random.int len)  in
  

  let myf2 = func random_set2 in 
  let total_w = Array.fold_left (fun s r-> let (w,e) = evs.(r) in w +. s  ) 0.0 random_set2 in
  *) 
  (*
  let x = [|0.00001;0.00001|] in
  let (errcode,loss2) = Lbfgs.minimize_l1 c myf x 0.0001 10 in
*)
  let x = [|0.00001;0.00001|] in
  try
    let (errcode,loss) = Lbfgs.minimize_l1 c func x 0.0001 10 in

  (*let (errcode,loss) = Lbfgs.minimize_l1 0.0 f x 1.0e-10 100 in*)
  (* DEBUG *)



  (*
   if log_exists log_debug then
    dlogf "gain = %f  (%s)\n" (-.loss) (Lbfgs.errstring errcode);
  *)
  
  vlogf "gain = %f  (%s) w0 = %f w1 = %f\n" (-.loss)   (Lbfgs.errstring errcode) x.(0) x.(1);
  
  
  if isnan loss then (0.0, (0.0, 0.0)) else 
  begin
    (* 
    let prior = lognormal prior_stddev x.(0)
           +. lognormal prior_stddev x.(1) in
              

    let nloss = (loss +. prior) *. (n /. total_w ) +. prior in
  
    vlogf "ngain = %f  (%s) n:%f w:%f\n" (-.nloss -. !nprior)  (Lbfgs.errstring errcode) n total_w;
    *)
    (-.loss , (x.(0), x.(1)))
  end

  with Prune_exception -> begin printf "Prune Exception:) \n"; exit 1; (0.0, (0.0, 0.0)) end
  end


(* Compute change in log likelihood when only the weights w_j and
   w_k of two mutually exclusive features f_j and f_k change *)
let fscore_2d2 c prior_stddev n evs extra_evs_schema f filtered_vars delta_logzs true_probs model_probs v  =
  (*if Circuit.feature_contains_var f v || List.exists (fun v'->(var_var v') = (var_var v)) filtered_vars  then (* || extra_evs_schema.(var_var v) >= 0 then *) *) 
  
  if Circuit.feature_contains_var f v || ((Array.length filtered_vars > 0) && (filtered_vars.(var_var v) > 0))  then (* || extra_evs_schema.(var_var v) >= 0 then *)  
    (0.0, (0.0, 0.0))
  else begin 
  let (var, value) = var_details v in
  vlogf "fscore2d2:  %s %d %d\n"  (string_of_feature f) var value; 
  (*print_string "l1: "; print_float c; print_string "\n";
  *)
  (*  
  let n_pt_d = ref 0.0 in 
  let n_pt_w = ref 0.0 in 
  let n_pf_d = ref 0.0 in 
  let n_pf_w = ref 0.0 in 
  *)
  let n = Array.length evs in 
  let it = ref 1 in
  let store_deltaz = (Array.length delta_logzs) > 0 in
  let func  delta gradient = 
    (* Compute value *)
    let value = ref 0.0 in
    let total_pt_data = ref 0.0 in
    let total_pf_data = ref 0.0 in
    let total_pt_w' = ref 0.0 in
    let total_pf_w' = ref 0.0 in 
    
    (*
    n_pt_d := 0.0;
    n_pt_w := 0.0;
    n_pf_w := 0.0;
    n_pf_d := 0.0;
  
    *)

    (*let (pt_data, pf_data) = true_probs in *)
    
    for j = 0 to n - 1 do 
    
      let pt_data = true_probs.(j).(0) in
      let pf_data = true_probs.(j).(1) in

      let (w,e, e') =  evs.(j) in
      let pt_w = model_probs.(j).(0) and pf_w = model_probs.(j).(1) in
      let pt_w' = pt_w *. exp delta.(0)  in
      let pf_w' = pf_w *. exp delta.(1)   in

      let log_delta_z = log (pt_w' +. pf_w' +. 1. -. pt_w -. pf_w) in
      if store_deltaz then delta_logzs.(j) <- log_delta_z;
      (*n_pt_w := !n_pt_w +. w *.  pt_w;
      n_pt_d := !n_pt_d +. w *.  pt_data;
      n_pf_w := !n_pf_w +. w *.  pf_w;
      n_pf_d := !n_pf_d +. w *.  pf_data;
      *)

      (*if isnan !value then  begin 
        printf "j=%d z'=%f pt_w:%f pf_w:%f t1:%0.3f t2:%0.3f\n" j log_delta_z pt_w pf_w delta.(0) delta.(1); 
      end;
      *)
      total_pt_w' :=  !total_pt_w' +. w *. (pt_w' /. (exp log_delta_z) );
      total_pf_w' :=  !total_pf_w' +. w *. (pf_w' /. (exp log_delta_z) );
      total_pt_data := !total_pt_data +. pt_data *. w;
      total_pf_data := !total_pf_data +. pf_data *. w;

      let raw_value = delta.(0) *. pt_data +. delta.(1) *. pf_data -. log_delta_z in (*(pt_w' +. pf_w' -. log z') in *)
      (*printf "j = %d ptdata = %f pfdata = %f w = %f\n" j pt_data pf_data w ; 
      *)
      (*printf "rawval: %f\n" raw_value;
        *)  
       (*delta.(0) *. pt_data +. delta.(1) *. pf_data -. log z' in *)
      
      (*if isnan raw_value then begin
        printf "ERROR: raw value is nan\n";
        printf "pt_data:%0.5f pf_data:%0.5f delta0:%0.5f delta1:%0.5f log_delta_z:%0.5f\n" pt_data pf_data delta.(0) delta.(1) log_delta_z;
      end;*)
      value := !value -. w *. raw_value; 
      (*dlogf "i:%d in grad, j:%d value:%0.5f rawvalue:%0.5f delta0:%0.5f delta1:%0.5f w:%0.1f pt_w:%f pf_w:%f pt_d:%f pf_d:%f lz:%f\n" !it j !value raw_value delta.(0) delta.(1) w pt_w pf_w pt_data pf_data log_delta_z; *)
    done;

    let prior = lognormal prior_stddev delta.(0)
          +. lognormal prior_stddev delta.(1) in
   
   
    
    let dprior_t = dlognormal prior_stddev delta.(0) in
    let dprior_f = dlognormal prior_stddev delta.(1) in
    
    (*printf "ptdata:%f, pfdata:%f\n tpt:%f tpf:%f\n" pt_data pf_data !total_pt_w' !total_pf_w';
    *)
    let dft_dx = (!total_pt_data) -. !total_pt_w' in
    let dff_dx = (!total_pf_data) -. !total_pf_w' in
    gradient.(0) <- -.(dft_dx +. dprior_t);
    gradient.(1) <- -.(dff_dx +. dprior_f);
    (*let gradient_mag = absf(gradient.(0)) +. absf(gradient.(1))  in
    *)
    (*if !it = 1 && gradient_mag < 0.1 then raise Prune_exception;
     *)
    value:= !value  -. prior; (* -. gradient.(0)  *);
    
    incr it;
    
    (*dlogf "grad.0=%0.6f grad.1=%0.6f value=%0.6f\n" gradient.(0) gradient.(1) !value;  
    *)
    (* Compute gradient *)
     (*printf "value in gradient:%0.5f g:%0.5f %0.5f prior:%0.3f\n" !value gradient.(0) gradient.(1) prior;
    *)
    
    (*let gradient_mag = absf(gradient.(0)) +. absf(gradient.(1))  in
    *)
    (*printf "gradient mag:%f\n" gradient_mag;
    *)
    !value 

    in

  (*let (w,ev) = evs.(0) in *)

  let nprior = ref 0.0  in 
  (*if ev.(var) >= 0 then nprior := 100.0; *) 
  (* Maximize log likelihood gain by minimizing its negative with LBFGS *)
  let len = Array.length evs in
  
  (*let random_set = Array.init (len) (fun i -> i)  in
  let myf = func random_set in 
  *)
  (*let random_set2 = Array.init (len/1) (fun i -> Random.int len)  in
  

  let myf2 = func random_set2 in 
  let total_w = Array.fold_left (fun s r-> let (w,e) = evs.(r) in w +. s  ) 0.0 random_set2 in
  *) 
  (*
  let x = [|0.00001;0.00001|] in
  let (errcode,loss2) = Lbfgs.minimize_l1 c myf x 0.0001 10 in
*)
  let x = [|0.00001;0.00001|] in
  try
    let (errcode,loss) = Lbfgs.minimize_l1 c func x 0.0001 10 in

  (*let (errcode,loss) = Lbfgs.minimize_l1 0.0 f x 1.0e-10 100 in*)
  (* DEBUG *)



  (*
   if log_exists log_debug then
    dlogf "gain = %f  (%s)\n" (-.loss) (Lbfgs.errstring errcode);
  *)
 
  let total_t = ref 0.0 in
  let total_f = ref 0.0 in 
  for j = 0 to Array.length evs - 1 do
    let (w,_,_) = evs.(j) in
    total_t := !total_t +. w *. true_probs.(j).(0);
    total_f := !total_f +. w *. true_probs.(j).(1);
  done;


    
  vlogf "gain = %f  (%s) w0 = %f w1 = %f counts: %f %f\n" (-.loss)   (Lbfgs.errstring errcode) x.(0) x.(1) !total_t !total_f;
  
  if isnan loss then (0.0, (0.0, 0.0)) else 
  begin
    (* 
    let prior = lognormal prior_stddev x.(0)
           +. lognormal prior_stddev x.(1) in
              

    let nloss = (loss +. prior) *. (n /. total_w ) +. prior in
  
    vlogf "ngain = %f  (%s) n:%f w:%f\n" (-.nloss -. !nprior)  (Lbfgs.errstring errcode) n total_w;
    *)
    (-.loss , (x.(0), x.(1)))
  end

  with Prune_exception -> begin printf "Prune Exception:) \n"; exit 1; (0.0, (0.0, 0.0)) end
  end

(* 
(* Simpler scoring method that only changes the weight of the first feature. *)
let fscore_1d prior_stddev n (p_data, pf_data) (p_w, pf_w) =
  (* Compute change in log likelihood when only the weight w_j of
     feature f_j changes, so w'_j = w_j + delta_j: 

     log P_w(X) - log P_w'(X) = 
       delta_j f_j(X) - log (P_w(f_j) exp(delta_j) + (1-P_w(f_j))) 
     d/d delta_j log P_w(X) - log P_w'(X) = 
       f_j(X) - (P_w(f_j) exp(delta_j))/(P_w(f_j) exp(delta_j) + (1-P_w(f_j))) *)
  dlogf "true: %f  exp: %f\n" p_data p_w;
  let f delta gradient = 
    let p_w' = p_w *. exp delta.(0) in
    let dprior = dlognormal prior_stddev delta.(0) in
    let dfdx = p_data -. p_w' /. (p_w' +. 1. -. p_w) in
    gradient.(0) <- -.(n *. dfdx +. dprior);
    let value = -.(delta.(0) *. p_data -. log (p_w' +. 1. -. p_w)) *. n
        -. lognormal prior_stddev delta.(0) in
    (* dlogf "x=%f  f(x)=%f  df/dx=%f\n" delta.(0) value gradient.(0); *)
    value in

  (* Maximize log likelihood gain by minimizing its negative with LBFGS *)
  let x = [|0.|] in
  let (errcode,loss) = Lbfgs.minimizep f x 1.0e-10 100 in
  (* DEBUG *)
  dlogf "gain = %f  (%d)\n" (-.loss) errcode;
  (-.loss, (x.(0), 0.))
  *)

(*

let fscore_2d2 schema cl c prior_stddev n evs extra_evs_schema f true_probs model_probs v  =
  
  let (var, value) = var_details v in
  vlogf "fscore:  %s %d %d\n"  (string_of_feature f) var value; 
  if Circuit.feature_contains_var f v then (* || extra_evs_schema.(var_var v) >= 0 then *)  
    begin
    printf "feature contains the var\n";
    (0.0, (0.0, 0.0))
  end
  else begin 
   
    (*let fev = conditions_to_ev schema (Array.of_list f.cond) in
    let redun v' = fev.(var_var v').(1 - (var_value v'))<-log_zero; let res= (List.exists (fun c-> ev_subset fev c) cl) in fev.(var_var v').(1 - (var_value v'))<-log_one; res in
    if redun v then begin printf "feature is redundant\n"; (0.0, (0.0, 0.0)) end 
    *)

    (*let fcondar = (Array.of_list f.cond) in
    let (s,v',va) = fcondar.((Array.length fcondar) - 1) in
    if var_var v > v' then begin
      printf "feature violates the ordering\n";
      (0.0, (0.0, 0.0))

      end
    else *)
  if true then
  begin 


  (*print_string "l1: "; print_float c; print_string "\n";
  *)
  (*
  let n_pt_d = ref 0.0 in 
  let n_pt_w = ref 0.0 in 
  let n_pf_d = ref 0.0 in 
  let n_pf_w = ref 0.0 in 
  *)
  let it = ref 1 in
  let func  delta gradient = 
    (* Compute value *)
    let g = gradient.(0) in
    let value = ref 0.0 in
    let total_pt_data = ref 0.0 in
    let total_pf_data = ref 0.0 in
    let total_pt_w' = ref 0.0 in
    let total_pf_w' = ref 0.0 in 
    (*
    n_pt_d := 0.0;
    n_pt_w := 0.0;
    n_pf_w := 0.0;
    n_pf_d := 0.0;
  *)
    let (pt_data, pf_data) = true_probs in
    for j = 0 to (Array.length evs) - 1 do 
      let (w,e) =  evs.(j) in
      let pt_w = model_probs.(j).(0) and pf_w = model_probs.(j).(1) in
      let pt_w' = pt_w *. exp delta.(0)  in
      let pf_w' = pf_w *. exp delta.(1)   in

      let log_delta_z = log (pt_w' +. pf_w' +. 1. -. pt_w -. pf_w) in
     (* 
      n_pt_w := !n_pt_w +. w *.  pt_w;
      n_pt_d := !n_pt_d +. w *.  pt_data;
      n_pf_w := !n_pf_w +. w *.  pf_w;
      n_pf_d := !n_pf_d +. w *.  pf_data;
      *)

      (*if isnan !value then  begin 
        printf "j=%d z'=%f pt_w:%f pf_w:%f t1:%0.3f t2:%0.3f\n" j log_delta_z pt_w pf_w delta.(0) delta.(1); 
      end;
      *)
      total_pt_w' :=  !total_pt_w' +. w *. (pt_w' /. (exp log_delta_z) );
      total_pf_w' :=  !total_pf_w' +. w *. (pf_w' /. (exp log_delta_z) );
      total_pt_data := !total_pt_data +. pt_data *. w;
      total_pf_data := !total_pf_data +. pf_data *. w;

      let raw_value = delta.(0) *. pt_data +. delta.(1) *. pf_data -. log_delta_z in (*(pt_w' +. pf_w' -. log z') in *)
      
      (*printf "rawval: %f\n" raw_value;
        *)  
       (*delta.(0) *. pt_data +. delta.(1) *. pf_data -. log z' in *)
      
      (*if isnan raw_value then begin
        printf "ERROR: raw value is nan\n";
        printf "pt_data:%0.5f pf_data:%0.5f delta0:%0.5f delta1:%0.5f log_delta_z:%0.5f\n" pt_data pf_data delta.(0) delta.(1) log_delta_z;
      end;*)
      value := !value -. w *. raw_value; 
      dlogf "i:%d in grad, j:%d value:%0.5f rawvalue:%0.5f delta0:%0.5f delta1:%0.5f w:%0.1f pt_w:%f pf_w:%f pt_d:%f pf_d:%f lz:%f\n" !it j !value raw_value delta.(0) delta.(1) w pt_w pf_w pt_data pf_data log_delta_z;
    done;

    let prior = lognormal prior_stddev delta.(0)
          +. lognormal prior_stddev delta.(1) in
   
   
    
    let dprior_t = dlognormal prior_stddev delta.(0) in
    let dprior_f = dlognormal prior_stddev delta.(1) in
    
    (*printf "ptdata:%f, pfdata:%f\n tpt:%f tpf:%f\n" pt_data pf_data !total_pt_w' !total_pf_w';
    *)
    let dft_dx = (!total_pt_data) -. !total_pt_w' in
    let dff_dx = (!total_pf_data) -. !total_pf_w' in
    gradient.(0) <- -.(dft_dx +. dprior_t);
    gradient.(1) <- -.(dff_dx +. dprior_f);
    let gradient_mag = absf(gradient.(0)) +. absf(gradient.(1))  in
    
    (*if !it = 1 && gradient_mag < 0.1 then raise Prune_exception;
     *)
    value:= !value  -. prior; (* -. gradient.(0)  *);
    
    incr it;
    
    dlogf "grad.0=%0.6f grad.1=%0.6f value=%0.6f\n" gradient.(0) gradient.(1) !value;  
    (* Compute gradient *)
     (*printf "value in gradient:%0.5f g:%0.5f %0.5f prior:%0.3f\n" !value gradient.(0) gradient.(1) prior;
    *)
    
    let gradient_mag = absf(gradient.(0)) +. absf(gradient.(1))  in
    
    (*printf "gradient mag:%f\n" gradient_mag;
    *)
    !value 

    in

  let (w,ev) = evs.(0) in

  let nprior = ref 0.0  in 
  (*if ev.(var) >= 0 then nprior := 100.0; *) 
  (* Maximize log likelihood gain by minimizing its negative with LBFGS *)
  let len = Array.length evs in
  
  (*let random_set = Array.init (len) (fun i -> i)  in
  let myf = func random_set in 
  *)
  (*let random_set2 = Array.init (len/1) (fun i -> Random.int len)  in
  

  let myf2 = func random_set2 in 
  let total_w = Array.fold_left (fun s r-> let (w,e) = evs.(r) in w +. s  ) 0.0 random_set2 in
  *) 
  (*
  let x = [|0.00001;0.00001|] in
  let (errcode,loss2) = Lbfgs.minimize_l1 c myf x 0.0001 10 in
*)
  let x = [|0.00001;0.00001|] in
  try
    let (errcode,loss) = Lbfgs.minimize_l1 c func x 0.0001 10 in

  (*let (errcode,loss) = Lbfgs.minimize_l1 0.0 f x 1.0e-10 100 in*)
  (* DEBUG *)



  (*
   if log_exists log_debug then
    dlogf "gain = %f  (%s)\n" (-.loss) (Lbfgs.errstring errcode);
  *)
  
  vlogf "gain = %f  (%s)\n" (-.loss)   (Lbfgs.errstring errcode);
  
  
  if isnan loss then (0.0, (0.0, 0.0)) else 
  begin
    (* 
    let prior = lognormal prior_stddev x.(0)
           +. lognormal prior_stddev x.(1) in
              

    let nloss = (loss +. prior) *. (n /. total_w ) +. prior in
  
    vlogf "ngain = %f  (%s) n:%f w:%f\n" (-.nloss -. !nprior)  (Lbfgs.errstring errcode) n total_w;
    *)
    (-.loss , (x.(0), x.(1)))
  end

  with Prune_exception -> begin printf "Prune Exception:) \n"; exit 1; (0.0, (0.0, 0.0)) end

  end 
  end
*)
(* 
(* Simpler scoring method that only changes the weight of the first feature. *)
let fscore_1d prior_stddev n (p_data, pf_data) (p_w, pf_w) =
  (* Compute change in log likelihood when only the weight w_j of
     feature f_j changes, so w'_j = w_j + delta_j: 

     log P_w(X) - log P_w'(X) = 
       delta_j f_j(X) - log (P_w(f_j) exp(delta_j) + (1-P_w(f_j))) 
     d/d delta_j log P_w(X) - log P_w'(X) = 
       f_j(X) - (P_w(f_j) exp(delta_j))/(P_w(f_j) exp(delta_j) + (1-P_w(f_j))) *)
  dlogf "true: %f  exp: %f\n" p_data p_w;
  let f delta gradient = 
    let p_w' = p_w *. exp delta.(0) in
    let dprior = dlognormal prior_stddev delta.(0) in
    let dfdx = p_data -. p_w' /. (p_w' +. 1. -. p_w) in
    gradient.(0) <- -.(n *. dfdx +. dprior);
    let value = -.(delta.(0) *. p_data -. log (p_w' +. 1. -. p_w)) *. n
        -. lognormal prior_stddev delta.(0) in
    (* dlogf "x=%f  f(x)=%f  df/dx=%f\n" delta.(0) value gradient.(0); *)
    value in

  (* Maximize log likelihood gain by minimizing its negative with LBFGS *)
  let x = [|0.|] in
  let (errcode,loss) = Lbfgs.minimizep f x 1.0e-10 100 in
  (* DEBUG *)
  dlogf "gain = %f  (%d)\n" (-.loss) errcode;
  (-.loss, (x.(0), 0.))
  *)






let score_feature_extensions fl c prior_stddev circ ev_logzs data f vars evs extra_evs_schema ev_matched_data_list approx_percen=
  vlogf "score_feature_extensions: %s\n" (string_of_feature f);

  (*vlogf "expected_fx_probs %s\n" (string_of_feature f) ;
  *)
  Timer.start "emp_probs";
  
  (*let data_probs = empirical_fx_probs circ f data vars in 
  *)

  let (data_probs2, fprobs) =  empirical_fx_probs3 circ f data vars evs ev_matched_data_list in
  

  List.iter2 (
    fun c v ->
      let s = ref 0.0 in
      let s2 = ref 0.0 in
      let fprob = ref 0.0 in
      let var = var_var  v in
      for j = 0 to Array.length evs - 1 do
        let (w,_,_) = evs.(j) in
        fprob := !fprob +. w *. fprobs.(j);
        s := w *. c.(j).(0) +. !s;
        s2 := w *. c.(j).(1) +. !s2;
      done;
      vlogf "feature: %s var: %d counts: %f counts1:%f fprob:%f\n" (string_of_feature f) var !s !s2 !fprob;
    ) data_probs2 vars; 
  (*let v1_prob = List.hd data_probs in
  let v1_ev_prob = List.hd data_probs2 in

  let tfinal = ref 0.0 and ffinal = ref 0.0 in
  let n = ref 0.0 in
  for j = 0 to (Array.length evs) - 1 do
    let (w,x,e) = evs.(j) in 
    n := !n+.w; 
    tfinal := !tfinal +. v1_ev_prob.(j).(1) *. w;
    ffinal := !ffinal +. v1_ev_prob.(j).(0) *. w;
  done;
  printf "evtfinal: %f evffinal:%f otfinal:%f offinal:%f\n" (!tfinal /. !n) (!ffinal /. !n) (fst v1_prob) (snd v1_prob);
  *)
  Timer.stop "emp_probs";
  Timer.start "exp_probs";
  let model_probs  = expected_fx_probs circ f vars ev_logzs in
  Timer.stop "exp_probs";
  let n = float_of_int (Array.length data) in
  Timer.start "score_2d";
  (*let ret2 = List.map2 ( fun d m->let (d1,d2) = d and m1 = m.(0) and m2 = m.(1) in (absf(d1 -. m1), absf (d2 -. m2))) data_probs model_probs in
  *)
  (*
  let vscore = List.map2 (fun s v->(s,v)) scores vars in
  let vscore_ar = Array.of_list vscore in
  Array.sort (fun (s,v) (s',v')-> if s > s' then 1 else if s < s' then -1 else 0) vscore_ar;
  let len = Array.length vscore_ar in
  let top = (100 - approx_percen) * len / 100 in (*3 * len / 4 in *) 
  
  (*let filtered_vars = ref [] in
  for i = 0 to top do
    filtered_vars := (snd vscore_ar.(i)) :: !filtered_vars;
  done;
  *)

  let filtered_vars = Array.make (Array.length circ.schema) 0 in
  for i = 0 to top do
     filtered_vars.(var_var (snd vscore_ar.(i))) <- 1;
  done;
  *)
  (*
  let cl = List.map (fun f->conditions_to_ev circ.schema (Array.of_list f.cond)) fl in 
  *)
  (*let fev = conditions_to_ev schema (Array.of_list f.cond) in
  let redun v = fev.(var_var v).(1 - (var_value v))<-log_zero; let res=  (List.exists (fun c-> ev_subset fev c) cl) in fev.(var_var v).(1 - (var_value v))<-log_one; res in
  flush_all(); *)

   
  let real_score = List.map3 (fscore_2d2 c prior_stddev n evs extra_evs_schema f [||] [||]) data_probs2 model_probs vars in

  Timer.stop "score_2d";
  (* 
  List.iter3 (fun r g v-> vlogf "loss: %f estimated score: %f var: %d\n" (fst r) g (var_var v)) real_score scores vars;
  *)
  
  (real_score, data_probs2, model_probs)
