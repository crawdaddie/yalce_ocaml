open Yalce

let () = start_audio();;


let _ = lfnoise 20. 20. 500.
  |> ~> (sq 200.)
  |> play;;

Unix.sleep 5;
