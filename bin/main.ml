open Yalce

let _sq_input n = 
  let sq_node = sq 200. in
  pipe_output n sq_node;;

let sin_input n = 
  let sq_node = sin 200. in
  pipe_output n sq_node;;

let sin_input_ freq _n = 
  sin freq
;;

maketable_sin ();
start_audio ();


let _synth1 () = 
  let open Synth in
  let (synth, ch) = ch @@
    rand_choice 6. [220.0;
      246.94;
      261.63;
      293.66;
      329.63;
      349.23;
      391.99;
      880.0]
    => sin_input
    => tanh_node 3.
    => freeverb_node in
    
  (synth, ch)
    => sin_input_ 2.
    => mul synth
    |> play
in


let synth2 () = 
  let sa, finish = synth_new () in
  let sin1 = sa @@ sin 100. in
  let sin2 = sa @@ sin 800. in
  let sin3 = sa @@ sin 2. in
  let summed = sa @@ sin1 +~ sin2 in 
  let summed = sa @@ summed *~ sin3 in
  let summed = sa @@ mul_scalar 0.2 summed in
  finish summed
in    


let _out = synth2 () in

(* Do other work here *)
Thread.delay 100.0; (* Let the main thread run for 5 seconds *)

