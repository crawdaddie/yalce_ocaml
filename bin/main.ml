open Yalce

let sin_input n = 
  let sq_node = sin 200. in
  pipe_output n sq_node;;

let sq_input n = 
  let sq_node = sq 200. in
  pipe_output n sq_node;;


maketable_sin ();
start_audio ();

let __synth2 () = 
  let ( ~> ), finish = synth_new () in
  let sin1 = ~> (sin 100.) in
  let sin2 = ~> (sin 101.) in
  let sin3 = ~> (sin 8000.) in
  let summed = ~> (sumn [sin1; sin2; sin3]) in 
  let summed = ~> (mul sin1 summed) in
  let summed = ~> (mul_scalar 0.2 summed) in
  finish summed
in    

let bass () = 
  let choices = [
    110.0
    ; 165.0
    ; 330.0
    ; 110.0
    ; 261.63
    ; 349.23
   (* ; 391.99 *)
  ] in

  let ( ~> ), finish = synth_new () in
  let r = ~> (rand_choice 1.25 @@ List.map (fun x -> x *. 0.5) choices) in
  let sin1 = ~> (sin_input r) in
  let sin1 = ~> (tanh_node 2. sin1) in
  finish sin1
in

let synth1 () = 

  let choices = [
    110.0
    ; 165.0
    ; 330.0
    ; 261.63
    ; 349.23
    ; 880.0
    ; 1320.0
   ; 391.99 ] in

  let ( ~> ), finish = synth_new () in
  let r = ~> (rand_choice 6. choices) in
  let sq1 = ~> (sq_input r) in

  let amp_mod = ~> (sin 2.) in 
  let sq1 = ~> (mul sq1 amp_mod) in
  finish sq1
in

let reverb nodes =
  let ( ~> ), finish = synth_new () in  
  let s = ~> (sumn nodes) in
  let s = ~> (mul_scalar 0.2 s) in
  let r = ~> (freeverb_node s) in
  finish r
in

let _ = reverb [synth1 (); bass ()] in

Thread.delay 100.0; (* Let the main thread run for 5 seconds *)

