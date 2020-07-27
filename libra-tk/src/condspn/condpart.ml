(******************************************************************************)
(* The Libra Toolkit                                                          *)
(*                                                                            *)
(* Copyright (C) 2015 by Daniel Lowd and Amirmohammad Rooshenas               *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided that the following conditions are     *)
(* met:                                                                       *)
(*                                                                            *)
(* 1. Redistributions of source code must retain the above copyright          *)
(* notice, this list of conditions and the following disclaimer.              *)
(*                                                                            *)
(* 2. Redistributions in binary form must reproduce the above copyright       *)
(* notice, this list of conditions and the following disclaimer in the        *)
(* documentation and/or other materials provided with the distribution.       *)
(*                                                                            *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        *)
(* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          *)
(* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR      *)
(* A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT       *)
(* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      *)
(* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT           *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,      *)
(* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY      *)
(* THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT        *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE      *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.       *)
(******************************************************************************)

open Ext
open Printf
(*open Linear*)
open Sys
open Lbfgs
open Str

exception CutNotPossible 

let neg_inf = -. 1000000000.0 

let create_weight_vector dim = Array.make dim 0.0

let create_weight_matrix dimc dimx = 
	let m = Array.make_matrix dimc dimx 0.0 in
	for i=0 to dimc - 1 do
		for j = 0 to dimx - 1 do
			m.(i).(j) <- Random.float(1.000);
		done
	done;
	m

	(*let m = Array.make dimc [||] in
	for i = 0 to dimc - 1 do
		m.(i) <- Array.init dimx Random.float;
	done;
	m*)
(*
Array.make_matrix dimc dimx 0.0
 *)
(** z = \sum_{c'} exp(w_{c'}^T x) *)
let compute_z x w_mat =
	let wn = Array.length w_mat.(0) in
  let z = Array.fold_left (fun s wc-> s +. exp(adotprod_autobias x wc ) ) 0.0 w_mat in
  z 
    
exception IncorretProb

(** Pr(C=c|X=x, \theta) = \frac{exp(\theta_c^T x)}{z} *)
let pr_c_given_x x theta_c z =
  let num = exp ( adotprod_autobias x theta_c ) in
  let value= num /. z in
	if value < 0.0 then raise IncorretProb;
	value


exception EvDataMismatch

let update_w w l1 lambda data cluster_labels k numvars =
	let n = Array.length data in  
	dlogf "Learning weights for lr... num of samples:%d, num of evars:%d dim:%d\n" n numvars k;

  let nl = Array.length cluster_labels in
	if n <> nl then raise EvDataMismatch;
	let nf = float_of_int n in

 	let it = ref 0 in 
  let f flat_theta gradient =
		incr it;
    (** J(\theta) = -1/m [ \sum_i \sum_k 1{y_i = j} ln pr (y_i=j|x_i;\theta) ] + \lambda/2 \sum_i \sum_j \theta_{ij}^2 *)
    let bigJ_of_theta = ref 0.0 in
   
		for j = 0 to k - 1 do
      for v = 0 to numvars  do
        gradient.(j * (numvars+1) + v) <- 0.0;
      done
    done;
    let theta = Array.rev_flatten flat_theta k (numvars+1) in
    for i = 0 to n - 1 do
      let c = cluster_labels.(i) in
      let theta_c = theta.(c) in
      let x_i = data.(i) in
      let z_i = compute_z x_i theta in
      for j = 0 to k - 1 do 
        let pr_c = pr_c_given_x x_i theta_c z_i in
        if c = j then 
        begin
         	bigJ_of_theta := !bigJ_of_theta -. log(pr_c);
					if !bigJ_of_theta < 0.0 then 
					begin	
						printf "pr_c %f log pr_c: %f\n" (pr_c) (log pr_c);
						flush_all();
						exit 1;
					end;
          for v = 0 to numvars - 1 do
            let index = j * (numvars+1) + v in 
            gradient.(index) <- gradient.(index) -. (x_i.(v) *. (1.0 -. pr_c));
          done;
					let index = (j*(numvars+1)+numvars) in
          gradient.(index) <- gradient.(index) -. (1.0 -. pr_c);
        end
        else
        begin
          for v = 0 to numvars - 1 do
            let index = j * (numvars+1) + v in 
            gradient.(index) <- gradient.(index) -. (x_i.(v) *. (0.0 -. pr_c));
          done;
          let index = (j * (numvars+1)) + numvars  in
          gradient.(index) <- gradient.(index) -. (0.0 -. pr_c);
        end
				
      done
    done;
    let penalty = ref 0.0 in
    for j = 0 to k - 1 do
      for v = 0 to numvars do
        let index = (j * (numvars+1)) + v in 
        gradient.(index) <- (gradient.(index) /. nf) +. ((lambda *. flat_theta.(index))) ; 
        penalty := !penalty +. (flat_theta.(index) *. flat_theta.(index));
        w.(j).(v) <- flat_theta.(index);
      done
    done;
    penalty := !penalty *. (lambda /. 2.0);

    let finalbigJ_of_theta = (!bigJ_of_theta /. nf) +.( !penalty ) in
		dlogf "it=%d, J(theta) = %f penalty:%f final:%f\n" !it !bigJ_of_theta !penalty finalbigJ_of_theta;
    
    bigJ_of_theta := finalbigJ_of_theta;

		(*
		vlogf "it=%d, J(theta) = %f nf:%f\n" !it !bigJ_of_theta nf;
		*)
    !bigJ_of_theta 
		in
  
  let point = Array.make (k * (numvars+1)) 0.01 in 
  let (errcode, min) = Lbfgs.minimize_l1 l1 f point 1.0e-5 100 in 
  (*let (errcode, min) = Lbfgs.minimize_l1 ((float_of_int k) /. nf) f point 1.0e-5 100 in *) 
 	 
	
	(*Array.iter (printf "%0.3f ") point;
	printf "\n";
	dlogf "Learn LR weights. Done!\n"; *)
	let upoint = Array.rev_flatten point k (numvars+1) in	

	(*
	Array.iteri (fun i p->Array.iter (printf "%0.3f ") p; printf "\n%d\n" i) upoint;
	flush_all();
	*)
	
	for c=0 to k - 1	do
		dlogf "inupdate,c=%d," c;
		Array.iter (fun wi-> dlogf "%f " wi) w.(c);
		dlogf "\n";
	done;
	(min, upoint ) 



(* Computes #(x_i,c_k) for every variable *)
let get_counts counts_xi_ck prior data schema c_ar c numvars= 
  
  (*let create_counts i = Array.make_matrix schema.(i) c prior in
  let counts_xi_ck = Array.init numvars (create_counts) in
  *)

  (** Reset counts. *)
  for i = 0 to numvars - 1 do
    for j = 0 to schema.(i) - 1 do
      for k = 0 to c - 1 do
        counts_xi_ck.(i).(j).(k) <- 0.0
      done
    done
  done;
  (*counts_xi_ck.(0).(0).(0) <- 0.0; *)
  let counts_ck = Array.make c prior in 

  let add_counts k x =
    let ck = c_ar.(k) in
    counts_ck.(ck) <- counts_ck.(ck) +. 1.0;
    for i = 0 to numvars - 1 do
      let xi = x.(i) in
      counts_xi_ck.(i).(xi).(ck) <- counts_xi_ck.(i).(xi).(ck) +. 1.0; 
    done in
  Array.iteri (add_counts) data;
  (counts_ck, counts_xi_ck)


exception IncorrectProb

type em_speed = EM_FAST | EM_SLOW | EM_SLOWEST


(** Maps the EM speed to a set of predefined values for the number of runs and convergence error threshold. *)
let get_em_param speed =
  let maxrun = ref 0 in
  let err_thresh = ref 0.0 in

  if speed = EM_FAST then begin maxrun:= 10; err_thresh := 0.1 end
  else if speed = EM_SLOW then begin maxrun := 20; err_thresh := 0.01 end
  (* speed == EM_SLOWEST *)
  else begin maxrun:= 40; err_thresh := 0.001 end;
  (!maxrun, !err_thresh)

exception DATA_EV_Inconsistent

(* Run expectation-maximation to find the maximum likelihood clustering *)
let em_cond data data_schema ev evnums c lambda speed l1 l2 =
  vlogf "Running Em for k=%d v=%d n=%d ... \n" c (Array.length data.(0)) (Array.length data);
	let prior = 0.00001 in
  let n = Array.length data in
  let n_ev = Array.length ev in

  if n != n_ev then raise DATA_EV_Inconsistent;

  let nf = float_of_int n in
  let nf_ev = float_of_int n_ev in
  let rand i = Random.int c in
  let c_ar = (Array.init n rand) in
  (*let xc = data.(r) in *)
  let err = ref 10.0 in
  
  let numvars = Array.length data_schema in 

  let it = ref 0 in   
  let cf = float_of_int c in
  let numvarsf = float_of_int numvars in

  let clustering_llg = ref (10000000.0) in
  let cs = Array.range c in
 
  let create_counts i = Array.make_matrix data_schema.(i) c prior in
  let counts_xi_ck = Array.init numvars (create_counts) in
  


  let exmaple_llgs = Array.make n 0.0  in
  
  (*let c_new = Array.make n 0 in*) 
  
  let maxrun, err_thresh = get_em_param speed in
 	let w = ref [||] in 

  while !it < maxrun && !err > err_thresh do   
    incr it;
    let (counts_ck, counts_xi_ck) = get_counts counts_xi_ck prior data data_schema c_ar c numvars in
    

    let wmat = create_weight_matrix c (evnums+1) in
    let (min, w') = update_w wmat l1 l2 ev c_ar c evnums in
		w := w';

  

    (** log p(x|ev) = log \sum_c p(x|c) p(c|ev) *)
    let logp_x_given_ev x e =
  
     let logp_ck ck = log ( counts_ck.(ck) +. prior ) -. log ( nf +.( cf *. prior ) ) in



      (** log p(x|c_k) 
       * = \sum_{i\in V} log P(x^i_j | c_k) *)    
      let logp_x_given_ck ck = 
        let sumv = ref 0.0 in
        for i = 0 to numvars - 1 do
          let xi = x.(i) in
          let v = log ( counts_xi_ck.(i).(xi).(ck) +.prior ) -. log ( counts_ck.(ck) +. ((float_of_int data_schema.(i)) *. prior )  )  in
          sumv := !sumv +. v 
        done;
        !sumv in
      (** log p(ev|e_c)
       * = \sum_{i\in V} log P(ev^i_j | c_k) *)



      let z = compute_z e !w in   
      let logp_ck_given_ev ck = log (pr_c_given_x e !w.(ck) z) in 

      (** log p( x, c_k| ev) = log p(x | c_k) + log p(c_k | ev) *) 
      
      let logp_x_joint_c_given_ev ck = logp_x_given_ck ck +. logp_ck_given_ev ck in
      (*let logp_x_joint_c_given_ev ck = logp_x_given_ck ck +. logp_ck ck  in *)
      let ar = Array.map (logp_x_joint_c_given_ev) cs in 
      
      (** max_c = argmax_c p(x,c| ev) *)
      let max_c = Array.argmax ar in

      (*let res = Array.max ar in *)
      (* res = log \sum_c p(x,c|ev) *) 
      let res = Ext.alogsumexp ar in    
      
      if (res > 0.0 ) then
      begin
        Array.iter (dlogf "%f ") ar;
        dlogf "\n";
        Array.iter (fun lp->dlogf "%f " (exp lp)) ar;
        dlogf "\n";
        dlogf "res: %f max value: %f\n" res (Array.max ar ); raise IncorrectProb
        
      end; 
      (res, max_c) in
      
    let llg = ref 0.0 in

    for i = 0 to n - 1 do
      let (res, max_c) = logp_x_given_ev data.(i) ev.(i) in
      exmaple_llgs.(i) <- res;
      c_ar.(i) <- max_c;
      llg := !llg +. res 
    done;

    let penalty = (lambda *. cf *. numvarsf *. (log nf)  ) in

    let penalized_llg = !llg -. penalty in

    err := absf (penalized_llg -. !clustering_llg); 

    dlogf "Round %d #of clusters:%d clustering penalized llg:%f err:%f normalized_llg: %f\n" !it c (penalized_llg /. nf) !err (!llg /. nf);
    clustering_llg := penalized_llg
  done;
  (!clustering_llg, c_ar, !w)

let em_cond2 data data_schema ev evschema c lambda speed l1 l2 =
  vlogf "Running Em for k=%d v=%d n=%d ... \n" c (Array.length data.(0)) (Array.length data);
	let prior = 0.00001 in
  let n = Array.length data in
  let n_ev = Array.length ev in

  if n != n_ev then raise DATA_EV_Inconsistent;

  let nf = float_of_int n in
  let nf_ev = float_of_int n_ev in
  let rand i = Random.int c in
  let c_ar = (Array.init n rand) in
  (*let xc = data.(r) in *)
  let err = ref 10.0 in
  
  let numvars = Array.length data_schema in 

  let it = ref 0 in   
  let cf = float_of_int c in
  let numvarsf = float_of_int numvars in

  let clustering_llg = ref (10000000.0) in
  let cs = Array.range c in
 
  let create_counts i = Array.make_matrix data_schema.(i) c prior in
  let counts_xi_ck = Array.init numvars (create_counts) in
  


  let exmaple_llgs = Array.make n 0.0  in
  
  (*let c_new = Array.make n 0 in*) 
  
  let maxrun, err_thresh = get_em_param speed in
 	let w = ref [||] in 

  while !it < maxrun && !err > err_thresh do   
    incr it;
    let (counts_ck, counts_xi_ck) = get_counts counts_xi_ck prior data data_schema c_ar c numvars in
    

    let wmat = create_weight_matrix c (evnums+1) in
    let (min, w') = update_w wmat l1 l2 ev c_ar c evnums in
		w := w';

  

    (** log p(x|ev) = log \sum_c p(x|c) p(c|ev) *)
    let logp_x_given_ev x e =
  
     let logp_ck ck = log ( counts_ck.(ck) +. prior ) -. log ( nf +.( cf *. prior ) ) in



      (** log p(x|c_k) 
       * = \sum_{i\in V} log P(x^i_j | c_k) *)    
      let logp_x_given_ck ck = 
        let sumv = ref 0.0 in
        for i = 0 to numvars - 1 do
          let xi = x.(i) in
          let v = log ( counts_xi_ck.(i).(xi).(ck) +.prior ) -. log ( counts_ck.(ck) +. ((float_of_int data_schema.(i)) *. prior )  )  in
          sumv := !sumv +. v 
        done;
        !sumv in
      (** log p(ev|e_c)
       * = \sum_{i\in V} log P(ev^i_j | c_k) *)



      let z = compute_z e !w in   
      let logp_ck_given_ev ck = log (pr_c_given_x e !w.(ck) z) in 

      (** log p( x, c_k| ev) = log p(x | c_k) + log p(c_k | ev) *) 
      
      let logp_x_joint_c_given_ev ck = logp_x_given_ck ck +. logp_ck_given_ev ck in
      (*let logp_x_joint_c_given_ev ck =  logp_x_given_ck ck +. logp_ck ck  in *)
      let ar = Array.map (logp_x_joint_c_given_ev) cs in 
      
      (** max_c = argmax_c p(x,c| ev) *)
      let max_c = Array.argmax ar in

      (*let res = Array.max ar in *)
      (* res = log \sum_c p(x,c|ev) *) 
      let res = Ext.alogsumexp ar in    
      
      if (res > 0.0 ) then
      begin
        Array.iter (dlogf "%f ") ar;
        dlogf "\n";
        Array.iter (fun lp->dlogf "%f " (exp lp)) ar;
        dlogf "\n";
        dlogf "res: %f max value: %f\n" res (Array.max ar ); raise IncorrectProb
        
      end; 
      (res, max_c) in
      
    let llg = ref 0.0 in

    for i = 0 to n - 1 do
      let (res, max_c) = logp_x_given_ev data.(i) ev.(i) in
      exmaple_llgs.(i) <- res;
      c_ar.(i) <- max_c;
      llg := !llg +. res 
    done;

    let penalty = (lambda *. cf *. numvarsf *. (log nf)  ) in

    let penalized_llg = !llg -. penalty in

    err := absf (penalized_llg -. !clustering_llg); 

    dlogf "Round %d #of clusters:%d clustering penalized llg:%f err:%f normalized_llg: %f\n" !it c (penalized_llg /. nf) !err (!llg /. nf);
    clustering_llg := penalized_llg
  done;
  (!clustering_llg, c_ar, !w)




let em_cluster_parallel_cond nodeid data schema ev evnums c lambda concurrent_thr speed l1 l2=
  vlogf "em cond parallel clustering ...\n";
  let n = Array.length data in
  let llgs = Array.make c (neg_inf) in
  let child_list = ref [] in

  for i = 1 to c-1 do
    flush_all();
    let nconcurrents = ref (List.length !child_list) in
    while !nconcurrents >= concurrent_thr do
      let (pid, status) = Unix.wait() in
      child_list := List.rem_item !child_list pid;
      nconcurrents := !nconcurrents - 1
    done;
		
    let pid = Unix.fork() in
      match pid with
      	0 -> begin
       		let max_llg = ref (neg_inf) in
        	let b_cr = ref [||] in
        	let w_mat = ref [||] in
        	for j=1 to 1 do
          	let (cllg, c_ar, w) = em_cond data schema ev evnums (i+1) lambda speed l1 l2 in
         		if cllg > !max_llg then begin
            	max_llg := cllg;
              b_cr := c_ar;
              w_mat := w;
          	end
        	done; 
					
        	let name = sprintf "node-%d-%d.cluster" nodeid i in 
       		let out = open_out name in
        	fprintf out "%f\n" !max_llg;

        	Array.iter (fprintf out "%d, ") !b_cr;
        	fprintf out "\n";
        	for k=0 to i do 
          	Array.iter (fun w_i->fprintf out "%.8f, " w_i) !w_mat.(k);
          	fprintf out "\n"
        	done;
       	  close_out_noerr out;
       		exit 1;
				end
      | _ -> begin
        ignore()
      end;
		
    child_list := pid::!child_list
  	
	done;
  
  while (List.length !child_list) > 0 do
    let p = List.hd !child_list in 
    child_list := List.rem_first !child_list;
    ignore (Unix.waitpid [] p)
  done;

  for i = 1 to c - 1 do 
    let name = sprintf "node-%d-%d.cluster" nodeid i in 
    let f = open_in name in
    let line = input_line f in 
    let cllg = float_of_string line in
    close_in_noerr f; 
    llgs.(i) <- cllg 
  done;

  let bestC = Array.argmax llgs in 
  Array.iter (printf "%f ") llgs;
  printf "\n";
  
  (*let (cllg, bestC_ar) = em data schema (bestC+1) lambda in *)
  let name = sprintf "node-%d-%d.cluster" nodeid bestC in 
  let channel = open_in name in
  let comma = regexp "[ \t]*,[ \t]*" in
  let line1 = input_line channel in

  let cllg = float_of_string line1 in
   

  let line2 = input_line channel in

  let tokens = split comma line2 in
  
  let bestC_ar = Array.map (fun s -> int_of_string s) (Array.of_list tokens) in
  vlogf "bestC=%d cllg=%f\n" (bestC+1) (cllg /. (float_of_int n));
  flush_all();
	let w_mat = Array.make (bestC+1) [||] in  
  for k=0 to bestC do 
    let line = input_line channel in
    let tokens = split comma line in
    let w_k = Array.map (fun s -> (float_of_string s)) (Array.of_list tokens) in
    w_mat.(k) <- w_k;
  done;

	close_in_noerr channel;
  Timer.start "data_parts_merge";
  let data_parts = Array.make (bestC+1) [||] in
  let ev_parts = Array.make (bestC+1) [||] in
	
  for i=0 to n-1 do 
    let ck = bestC_ar.(i) in
    data_parts.(ck) <- Array.append data_parts.(ck) [|data.(i)|];
    ev_parts.(ck) <- Array.append ev_parts.(ck) [|ev.(i)|];
  done;
  let new_data_parts = ref ([||]) in
  let new_ev_parts = ref ([||]) in
  for i=0 to (Array.length data_parts) - 1 do
    if (Array.length data_parts.(i)) > 0 then
    begin
      new_data_parts := Array.append !new_data_parts [|data_parts.(i)|];
      new_ev_parts := Array.append !new_ev_parts [|ev_parts.(i)|];
    end
  done; 
  Timer.stop "data_parts_merge";
  (!new_data_parts, !new_ev_parts, w_mat)



(** Collect all pairwise counts *)
let counts schema data =
  let numvars = Array.length schema in
  let marg_counts = Array.map (fun d -> Array.make d 0.0) schema in
  let create_counts i j = Array.make_matrix schema.(i) schema.(j) 0.0 in
  let joint_counts = 
    Array.init numvars (fun i -> Array.init i (create_counts i)) in
  let add_counts x =
    for i = 0 to numvars - 1 do
      let xi = x.(i) in
      marg_counts.(i).(xi) <- marg_counts.(i).(xi) +. 1.0;
      for j = 0 to i - 1 do
        let xj = x.(j) in
        joint_counts.(i).(j).(xi).(xj) <- joint_counts.(i).(j).(xi).(xj) +. 1.0
      done;
    done in
  Array.iter (add_counts) data;
  (marg_counts, joint_counts, Array.length data)



(** Computes a matrix of mutual information scores *)
(* 
@param prior prior, which is used to avoid division by zero
@schema an array that contains the dimension of variables
@num_examples number of data samples
*)
let compute_mi prior schema num_examples marg_counts joint_counts =
 let total = prior +. float_of_int num_examples in
 let calc_mi i j =
    let mi = ref 0.0 in
    let ip = prior /. (float_of_int schema.(i)) in
    let jp = prior /. (float_of_int schema.(j)) in
    let ijp = prior /. (float_of_int (schema.(i) * schema.(j))) in
    for ival = 0 to schema.(i) - 1 do
      for jval = 0 to schema.(j) - 1 do
        let p_ij = 
          (ijp +.  joint_counts.(i).(j).(ival).(jval)) /. total in
        let p_i  = (ip +.  marg_counts.(i).(ival)) /. total in
        let p_j  = (jp +.  marg_counts.(j).(jval)) /. total in
        dlogf "P(x_%d = %d, x_%d = %d) = %f\n" i ival j jval p_ij; 
        if p_ij > 0. then
          mi := !mi +. p_ij *. log (p_ij /. (p_i *. p_j))
      done;
    done;
    !mi in
  (* Calculate all mutual informations *)
  let numvars = Array.length schema in
  (*let all_mi = Array.init numvars (fun i -> Array.init i (calc_mi i)) in
  *)
  let all = Array.make_matrix numvars numvars 0.0 in
  for i=0 to numvars - 1  do
    for j=0 to i - 1 do 
      all.(i).(j) <- calc_mi i j;
      (*printf "%d %d %f\n" i j all.(i).(j);*)
      all.(j).(i) <- all.(i).(j)
    done
  done;
  all

let fsquare a = a *. a 

let distance x y =
  let xf = Array.map float_of_int x in
  let fsquare a = a *. a in
  let f = Array.map2 ( fun a b -> fsquare ( a -. b ) ) xf y in
  let d = Array.sumf f in
  d /. 2.0





let cluster_data_cond nodeid data schema ev evnums c lambda concurrent_thr l1 l2=
  (*kmeans data c *)
  Timer.start "clustering_time";
  let (data_clusters,ev_clusters, w_mat) = em_cluster_parallel_cond nodeid data schema ev evnums c lambda concurrent_thr EM_FAST l1 l2 in
  Timer.stop "clustering_time";
  vlogf "cluster time:%f\n" (Timer.last_elapsed "clustering_time");
  (data_clusters, ev_clusters, w_mat)
  (*em_cluster valid  data schema c lambda*)


let mult_mat2vec m v = 
  let newv = Array.map (fun row-> let temp = Array.mapi (fun j x-> v.(j) *. x ) row in Array.sumf temp ) m in
  newv

let normalize v = 
  let sum = Array.sumf v in 
  let v_n = Array.map (fun x-> x /. sum) v in 
  v_n

let horizontal_cluster_cond nodeid data i_schema ev evnums part_num lambda concurrent_thr l1 l2=
  let schema = (Array.map (fst) i_schema) in
  cluster_data_cond nodeid data schema ev evnums part_num lambda concurrent_thr l1 l2 
  

let create_adj_matrix data schema delta = 
  let (marg_counts, joint_counts, n) = counts schema data in
  let bigw = compute_mi 0.0 schema n marg_counts joint_counts in
  let var_num = Array.length schema in
  let adj_mat = Array.make_matrix var_num var_num 0 in
  (*vlogf "delta = %f\n" delta; *)
  for i = 0 to var_num - 1 do
    for j = 0 to i - 1 do
      let g_value = bigw.(i).(j) (* *. mult *) in
      adj_mat.(i).(j) <- if ( g_value > delta) then 1 else 0;
      (*printf "big[%d,%d] = %f\n" i j bigw.(i).(j);  *)
      adj_mat.(j).(i) <- adj_mat.(i).(j)
    done
  done;
  adj_mat 

let get_neighbors adj_mat varnum n =
  let n_list = ref [] in
  for i = varnum - 1 downto 0 do 
    if adj_mat.(n).(i) == 1 then 
      n_list := (i)::!n_list;
    ignore()
  done;
  !n_list


type dfs_color = DFS_White | DFS_Gray | DFS_Black 


let rec dfs_visit adj_mat color_map varnum cc n = 
  (*printf "dfs_visit n = %d\n" n;
  flush_all();*)
  color_map.(n) <- DFS_Gray;
  let local_cc = ref cc in
  let n_list = get_neighbors adj_mat varnum n in
  
  let check i =
    (*printf "check i = %d \n" i; 
    flush_all();
    *)
    if color_map.(i) == DFS_White then 
    begin
      (*local_cc := List.append !local_cc [i];*)
      local_cc := i::!local_cc;
      local_cc := dfs_visit adj_mat color_map varnum !local_cc i
    end in

  (*printf "size of n_list = %d\n" (List.length n_list);
  flush_all();*)
  let n_list_size = List.length n_list in
  if n_list_size > 0 then 
  begin
    List.iter (fun i -> (*printf "here i= %d\n" i; flush_all(); *)check i; color_map.(i) <- DFS_Black; ignore()) n_list
  end;
  
  (*printf "end-dfs_visit node n = %d\n" n; 
  flush_all();*)
  !local_cc 

let get_ccs adj_mat varnum = 
  (*printf "get_ccs2\n";
  flush_all();*)
  let color_map = Array.make varnum DFS_White in
  let ccs = ref [||] in 
  for i=0 to varnum - 1 do 
    if (color_map.(i) = DFS_White) then 
    begin
      (*printf "Starting new cc at %i\n" i;
      flush_all();*)
      let cc = [i] in 
      let cc = dfs_visit adj_mat color_map varnum cc i in 
      ccs := Array.append !ccs [|cc|]
    end
  done;
  dlogf "Component sizes: ";
  Array.iter (fun c->dlogf "%d " (List.length c)) !ccs;
  dlogf "\n";
  (*printf "end-get_cc\n"; flush_all();*)
  !ccs 


let find_ccs data i_schema delta =
  (*printf "find_ccs\n";
  flush_all(); *)
  let schema = Array.map (fst) i_schema in
  let adj_mat = create_adj_matrix data schema delta in 
  let varnum = Array.length schema in
  let ccs = get_ccs adj_mat varnum in
   if (Array.length ccs) <= 1 then raise CutNotPossible;
  ccs


  
