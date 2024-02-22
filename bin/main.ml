open Yalce

let _sq_input n = 
  let sq_node = sq 200. in
  pipe_output n sq_node;;

let sin_input n = 
  let sq_node = sin 200. in
  pipe_output n sq_node;;

let sin_input_ freq _n = 
  sin freq in

maketable_sin ();
start_audio ();



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
  
let _out = (synth, ch)
  => sin_input_ 2.
  => mul synth
  |> play in



(* Do other work here *)
Thread.delay 100.0; (* Let the main thread run for 5 seconds *)
let buf = Signal.to_list @@ Node.out synth in
List.iter (fun x -> Printf.printf("%f, ") x) buf;







