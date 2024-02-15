let () = Yalce.start_audio();;

let noise = Yalce.lfnoise 20. 20. 500. ;;
let _ = Yalce.add noise;;
let sq_node = Yalce.sq 500. ;;
let _ = Yalce.add sq_node;;
let () = Yalce.pipe_output noise sq_node;;
let () = Yalce.play sq_node;;

Unix.sleep 5;
