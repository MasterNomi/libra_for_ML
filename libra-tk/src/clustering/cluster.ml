
open Ext
open Data
open Printf

let file = ref ""
let ofile = ref ""
let numpart = ref 2
let evfile = ref ""
let alg = ref ""
let xfile = ref ""

let usage = "Usage: cluster -i <file>"
let args = Arg.align
 ([("-i", Arg.Set_string file, " Data file");
   ("-ev", Arg.Set_string evfile, " Evidence file");
   ("-x", Arg.Set_string xfile, " Only evidence data");

   ("-alg", Arg.Set_string alg, " Clustering algorithms [kmeans|EM]");
   ("-k", Arg.Set_int numpart, " Number of clusters");
   ("-o", Arg.Set_string ofile, " Output file")]
   @ common_arguments)


type em_speed = EM_FAST | EM_SLOW | EM_SLOWEST

exception DATA_EV_Inconsistent
exception IncorrectProb


let create_weight_vector dim = Array.make dim 0.0

let create_weight_matrix dimc dimx = 
	let m = Array.make_matrix dimc dimx 0.0 in
	for i=0 to dimc - 1 do
		for j = 0 to dimx - 1 do
			m.(i).(j) <- Random.float(1.000);
		done
	done;
	m


(** z = \sum_{c'} exp(w_{c'}^T x) *)
let compute_z x w_mat =
  let z = Array.fold_left (fun s wc-> s +. exp(adotprod_autobias x wc ) ) 0.0 w_mat in
  z 
    

(** Pr(C=c|X=x, \theta) = \frac{exp(\theta_c^T x)}{z} *)
let pr_c_given_x x theta_c z =
  let num = exp ( adotprod_autobias x theta_c ) in
  let value= num /. z in
	if value < 0.0 then raise IncorrectProb;
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
  
  let point = Array.make (k * (numvars+1)) 0.001 in 
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




let get_em_param speed =
  let maxrun = ref 0 in
  let err_thresh = ref 0.0 in

  if speed = EM_FAST then begin maxrun:= 10; err_thresh := 0.1 end
  else if speed = EM_SLOW then begin maxrun := 20; err_thresh := 0.01 end
  (* speed == EM_SLOWEST *)
  else begin maxrun:= 40; err_thresh := 0.001 end;
  (!maxrun, !err_thresh)



(* Run expectation-maximation to find the maximum likelihood clustering *)
let em_cond_nb data data_schema ev ev_schema c speed  =
  vlogf "Running Em for k=%d v=%d n=%d ... \n" c (Array.length data.(0)) (Array.length data);
	let prior = 0.00001 in
  let n = Array.length data in

  let nf = float_of_int n in
  let rand i = Random.int c in
  let c_ar = (Array.init n rand) in
  (*let xc = data.(r) in *)
  let err = ref 10.0 in
  
  let numvars = Array.length data_schema in 

  let it = ref 0 in   
  let cf = float_of_int c in
  let cs = Array.range c in
  let clustering_llg = ref (10000000.0) in
 
  let create_counts i = Array.make_matrix data_schema.(i) c prior in
  let counts_vi_ck = Array.init numvars (create_counts) in
  


  let exmaple_llgs = Array.make n 0.0  in
  
  (*let c_new = Array.make n 0 in*) 
  
  let maxrun, err_thresh = get_em_param speed in

  while !it < maxrun && !err > err_thresh do   
    incr it;
    let (counts_ck, counts_vi_ck) = get_counts counts_vi_ck prior data data_schema c_ar c numvars in
    
    (*
    let wmat = create_weight_matrix c (evnums+1) in
    let (min, w') = update_w wmat l1 l2 ev c_ar c evnums in
		w := w';
    *)

    (** v = y,x
        \log p(y|x) = \log \sum_c p(v|c) p(c) - \log \sum_c p(x|c) p(c) **)
    let logp_y_given_x d = 
      
      let logp_ck ck = 
        log ( counts_ck.(ck) +. prior ) -. log ( nf +.( cf *. prior ) ) in
      
      let logp_v_given_ck ck = 
        let sumv = ref 0.0 in
        for j = 0 to numvars - 1 do
          let vi = d.(j) in
          let s = log ( counts_vi_ck.(j).(vi).(ck) +.prior ) -. log ( counts_ck.(ck) +. ((float_of_int data_schema.(j)) *. prior )  )  in
          sumv := !sumv +. s;
        done;
        !sumv +. logp_ck ck in 
    
      let logp_x_given_ck ck =
        let sumx = ref 0.0 in
        for j = 0 to numvars - 1 do
          if ev_schema.(j) >= 0 then begin
            let vi = d.(j) in
            let s = log ( counts_vi_ck.(j).(vi).(ck) +.prior ) -. log ( counts_ck.(ck) +. ((float_of_int data_schema.(j)) *. prior )  )  in
            sumx := !sumx +. s;
          end;
        done;
        !sumx +. logp_ck ck in
      

      (** \log P(Y|c)*P(X|c)*P(c) **)
      let num_ar = Array.map (logp_v_given_ck) cs in

      (** argmax P(c| Y,X) = argmax \log P(Y|c)P(X|c)P(c) **)
      let c_map = Array.argmax num_ar in
      
      (** \log P(X|c)P(c) **)
      let denum = Ext.alogsumexp (  (Array.map (logp_x_given_ck) cs) ) in
  

      (** \log P(Y|X) = \log (\sum_c P(V|c)P(c)) - \log P(X|c)P(c) **) 
      let logp = alogsumexp(num_ar) -. denum in 

      (logp, c_map) in
      
    
      
    (*
    (** log p(y|x) = log \sum_c p(x|c) p(c|ev) *)
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
      
    *)
    let llg = ref 0.0 in
    for i = 0 to n - 1 do
      let (res, max_c) = logp_y_given_x data.(i) in
      exmaple_llgs.(i) <- res;
      c_ar.(i) <- max_c;
      llg := !llg +. res 
    done;
    let penalty = 0.0 in
    (*let penalty = (lambda *. cf *. numvarsf *. (log nf)  ) in *)
    let penalized_llg = !llg -. penalty in
    err := absf (penalized_llg -. !clustering_llg); 
    dlogf "Round %d #of clusters:%d clustering penalized llg:%f err:%f normalized_llg: %f\n" !it c (penalized_llg /. nf) !err (!llg /. nf);
    clustering_llg := penalized_llg

  done;
  (!clustering_llg, c_ar)


(******************************************************)
(******************************************************)
(******************************************************)
(******************************************************)
(******************************************************)
(******************************************************)
(******************************************************)


let em_cond_nb_lr data data_schema ev c speed l1 l2 =
  vlogf "Running Em for k=%d v=%d n=%d ... \n" c (Array.length data.(0)) (Array.length data);
	let prior = 0.00001 in
  let n = Array.length data in

  let nf = float_of_int n in
  let rand i = Random.int c in
  let c_ar = (Array.init n rand) in
  (*let xc = data.(r) in *)
  let err = ref 10.0 in
  
  let numvars = Array.length data_schema in 
  let evnums = Array.length (ev.(0)) in
  let it = ref 0 in   
  let cf = float_of_int c in
  let cs = Array.range c in
  let clustering_llg = ref (10000000.0) in
 
  let create_counts i = Array.make_matrix data_schema.(i) c prior in
  let counts_vi_ck = Array.init numvars (create_counts) in
  


  let exmaple_llgs = Array.make n 0.0  in
  
  (*let c_new = Array.make n 0 in*) 
  
  let maxrun, err_thresh = get_em_param speed in

  let w = ref (create_weight_matrix c (evnums+1)) in
  while !it < maxrun && !err > err_thresh do   
    incr it;
    let (counts_ck, counts_vi_ck) = get_counts counts_vi_ck prior data data_schema c_ar c numvars in
    
    
    let wmat = create_weight_matrix c (evnums+1) in
    let (min, w') = update_w wmat l1 l2 ev c_ar c evnums in
		w := w';
    

    (** v = y,x
        \log p(y|x) = \log \sum_c p(v|c) p(c) - \log \sum_c p(x|c) p(c) **)
    let logp_y_given_x y x= 
      
      (*let logp_ck ck = 
        log ( counts_ck.(ck) +. prior ) -. log ( nf +.( cf *. prior ) ) in
      *)
      
      let z = compute_z x !w in  
      
      let logp_ck_given_x ck = 
        log(pr_c_given_x x !w.(ck) z) in
        

      let logp_y_given_ck ck = 
        let sumy = ref 0.0 in
        for j = 0 to numvars - 1 do
          let vi = y.(j) in
          let s = log ( counts_vi_ck.(j).(vi).(ck) +.prior ) -. log ( counts_ck.(ck) +. ((float_of_int data_schema.(j)) *. prior )  )  in
          sumy := !sumy +. s;
        done;
        !sumy in 
    
      (** \log P(Y|c)*P(c|x) **)
      let logp_ar = Array.map (fun ck->logp_y_given_ck ck +. logp_ck_given_x ck) cs in

      (** argmax P(c| Y,X) = argmax \log P(Y|c)P(c|x) **)
      let c_map = Array.argmax logp_ar in
      
  
      (** \log P(Y|X) = \log (\sum_c P(Y|c)*P(c|x) **) 
      let logp = alogsumexp(logp_ar) in 

      (logp, c_map) in
      
    
      
    (*
    (** log p(y|x) = log \sum_c p(x|c) p(c|ev) *)
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
      
    *)
    let llg = ref 0.0 in
    for i = 0 to n - 1 do
      let (res, max_c) = logp_y_given_x data.(i) ev.(i) in
      exmaple_llgs.(i) <- res;
      c_ar.(i) <- max_c;
      llg := !llg +. res 
    done;
    let penalty = 0.0 in
    (*let penalty = (lambda *. cf *. numvarsf *. (log nf)  ) in *)
    let penalized_llg = !llg -. penalty in
    err := absf (penalized_llg -. !clustering_llg); 
    dlogf "Round %d #of clusters:%d clustering penalized llg:%f err:%f normalized_llg: %f\n" !it c (penalized_llg /. nf) !err (!llg /. nf);
    clustering_llg := penalized_llg

  done;
  (!clustering_llg, c_ar)



let absf f = if f < 0.0 then -.f else f

let fsquare a = a *. a 

let distance x y =
  let xf = Array.map float_of_int x in
  let fsquare a = a *. a in
  let f = Array.map2 ( fun a b -> fsquare ( a -. b ) ) xf y in
  let d = Array.sumf f in
  d /. 2.0


let kmeans data c = 
  vlogf "Initialize clustering ... \n";

  let n = Array.length data in
  let x_c = Array.create c [||] in
  for k = 0 to c - 1 do
    let kc = Random.int (n-1) in 
    x_c.(k) <- Array.map (float_of_int) data.(kc)
  done;

  let data_parts = Array.make c [||] in
  let clusters = Array.make n 0 in
  let err = ref 1.0 in
  let it = ref 0 in
  while  !err > 0.1 && !it < 50 do
    incr it;
    for k = 0 to c - 1 do
      data_parts.(k) <- [||];
    done;
    err := 0.0;
    for j = 0 to n - 1 do   
      let x = data.(j) in
      let d = Array.map (fun x' -> distance x  x' ) x_c in
      let k = Array.argmin d in
      clusters.(j) <- k;
      data_parts.(k) <-  Array.append data_parts.(k) [|x|]
    done;


    let x_c' = Array.create c (Array.create (Array.length x_c.(0)) 0.0 ) in 
    for k = 0 to c - 1 do
      let c_size = Array.length data_parts.(k) in

      for j = 0 to c_size - 1 do
        let x = data_parts.(k).(j) in
        let x_f = Array.map (float_of_int) x in
      
        x_c'.(k) <- Array.map2 (fun a b -> a +. b) x_f x_c'.(k)
      done;
      x_c'.(k) <- Array.map (fun a-> a /. float_of_int c_size) x_c'.(k);
      
      let e = Array.sumf (Array.map2 ( fun a b -> absf ( a-. b) ) x_c.(k) x_c'.(k)) in
      err := !err +. e;
      x_c.(k) <- x_c'.(k);
    done;
    vlogf "kmean err = %f\n" !err;

  done;
  clusters



let main () =
  Arg.parse args ignore usage;
  if !file = ""  || !alg = "" then
    (Arg.usage args usage; exit 0);

  common_log_init();
  let data = Data.load_data !file in



  let schemafile = "" in 
   
  let schema = 
    if schemafile <> "" then begin
      let schemain = open_in schemafile in
      let s = Data.input_example schemain in
      close_in schemain ; s
    end
    else Data.schema data in 
  
  let data_ar = Array.of_list data in 

  printf "running alg = %s\n" !alg;
  if !alg = "EM" then begin
    let no_wev = Data.input_example_list (open_in !evfile) in
    let ev_schema = Array.map (fun i->i) (List.hd no_wev) in
    let (cll, c_ar) = em_cond_nb data_ar schema [||] ev_schema !numpart EM_SLOWEST in
    printf "cll = %f\n" (cll /. (float_of_int (Array.length data_ar)));
    ignore()
  end
  else if !alg = "Kmeans" then begin
    let clusters = kmeans data_ar !numpart in
    Data.dump_data [|clusters|] !ofile;
    ignore()
  end
  else if !alg = "EMLR" then begin
     let ev = List.map ( Array.map (float_of_int) ) (Data.input_example_list (open_in !evfile)) in
     let (cll, c_ar) = em_cond_nb_lr data_ar schema (Array.of_list ev) !numpart EM_SLOWEST 0.000001 1.0 in
     printf "cll = %f\n" (cll /. (float_of_int (Array.length data_ar)));
  end
  else printf "Unknown clustering algorithm. Choose among [EM|Kmeans|EMLR]\n";
  ignore()

  
 let _ = main()
