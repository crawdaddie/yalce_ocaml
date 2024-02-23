open Ctypes
open Foreign

type audio_ctx = unit ptr
let audio_ctx : audio_ctx typ = ptr void

(* typedef struct Signal { *)
(*   // size of the data array will be size * layout *)
(*   // data is interleaved, so sample for frame x, channel 0 will be at index *)
(*   // layout * x + 0 *)
(*   // sample for frame x, channel 1 will be at index layout * x + 1 *)
(*   double *data; *)
(*   int size;   // number of frames *)
(*   int layout; // how they are laid out *)
(* } Signal; *)
(* *)
(* typedef struct Node { *)
(*   enum { *)
(*     INTERMEDIATE = 0, *)
(*     OUTPUT, *)
(*   } type; *)
(* *)
(*   bool killed; *)
(* *)
(*   node_perform perform; *)
(*   void *data; *)
(*   Signal *ins; *)
(*   int num_ins; *)
(* *)
(*   Signal *out; *)
(* *)
(*   struct Node *next; *)
(*   struct Node *head; *)
(*   struct Node *tail; *)
(* } Node; *)
type node_type = INTERMEDIATE | OUTPUT
let of_int = function 0 -> INTERMEDIATE | 1 -> OUTPUT | _ -> raise (Invalid_argument "Unexpected C enum")
let to_int = function INTERMEDIATE -> 0 | OUTPUT -> 1 
let node_type = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

module Signal = struct
  type signal_struct
  let signal_struct : signal_struct structure typ = structure "Signal"
  let signal_buf = field signal_struct "buf" (ptr double)
  let signal_size = field signal_struct "size" int
  let signal_layout = field signal_struct "layout" int

  let () = seal signal_struct

  type signal 
  let signal = ptr signal_struct
  let buf signal = getf !@signal signal_buf
  let size signal = getf !@signal signal_size
  let layout signal = getf !@signal signal_layout


  let to_list signal =
    let buf_ptr = buf signal in
    let len = size signal in
    let carr = CArray.from_ptr buf_ptr len in
    CArray.to_list carr
    
end

module Node = struct
  type node_struct
  let node_struct : node_struct structure typ = structure "Node"
  let node_killed = field node_struct "killed" bool
  let node_typef = field node_struct "type" int
  let node_perform = field node_struct "node_perform" (ptr void)
  let node_state = field node_struct "state" (ptr void)
  let node_ins = field node_struct "ins" (Signal.signal)
  let node_num_ins = field node_struct "num_ins" int
  let node_out = field node_struct "out" (Signal.signal)

  let () = seal node_struct
  type node
  let node = ptr node_struct

  let out node = 
    getf !@node node_out
end





let start_audio = foreign "start_audio" (void @-> (returning void))
let get_audio_ctx = foreign "get_audio_ctx" (void @-> (returning audio_ctx)) 

let pipe_output = foreign "pipe_output" (Node.node @-> Node.node @-> (returning Node.node))
let sq = foreign "sq_node" (double @-> (returning Node.node))
let lfnoise = foreign "lfnoise" (double @-> double @-> double @-> (returning Node.node))
let rand_choice_ = foreign "rand_choice_node" (double @-> int @-> ptr double @-> (returning Node.node))
let sin = foreign "sine" (double @-> (returning Node.node))
let maketable_sin = foreign "maketable_sin" (void @-> (returning void))
let tanh_node = foreign "tanh_node" (double @-> Node.node @-> (returning Node.node))  
let freeverb_node = foreign "freeverb_node" (Node.node @-> (returning Node.node))  
let mul = foreign "mul_node" (Node.node @-> Node.node @-> (returning Node.node))
let mul_scalar = foreign "mul_scalar_node" (double @-> Node.node @-> (returning Node.node))
let sum_nodes_ = foreign "sum_nodes" (int @-> Node.node @-> Node.node @-> (returning Node.node))
let sum_nodes_arr = foreign "sum_nodes_arr" (int @-> ptr Node.node @-> (returning Node.node))




let ctx_add = foreign "ctx_add" (Node.node @-> (returning Node.node))
let add_to_chain = foreign "add_to_chain" (Node.node @-> Node.node @-> (returning Node.node))
let add_to_dac = foreign "add_to_dac" (Node.node @-> (returning Node.node))
let chain_new = foreign "chain_new" (void @-> (returning Node.node))

let synth_new () =
  let chain = chain_new () in
  let add = fun x ->
    add_to_chain chain @@ x
  in

  let fin = fun out -> 
    let _ = add_to_dac chain in
    let _ = ctx_add chain in
    out
  in
  (add, fin)
  


let rand_choice freq choices =
  let len = List.length choices in
  let choices_carr = CArray.of_list double choices in
  rand_choice_ freq len @@ CArray.start choices_carr

let node_list_to_carr_ptr nodes =
  let len = List.length nodes in
  len, CArray.start (CArray.of_list Node.node nodes)

let sumn nodes =
  let len, ptr = node_list_to_carr_ptr nodes in
  sum_nodes_arr len ptr

  


let pipe_input a b = pipe_output b a
let ( => ) = pipe_output
let ( =< ) = pipe_input

let sum2 x y = sumn [x; y]
let ( +~ ) = sum2
let ( *~ ) = mul



module Synth = struct
  let chain =
    chain_new ()

  let ch x =
    let chain = chain_new () in 
    (add_to_chain chain x, chain)

  let ( => ) m f = 
    let (x, ch) = m in
    let y = f x in
    (add_to_chain ch y, ch)

  let play m =
    let (_, ch) = m in
    let _ = add_to_dac ch in
    let _ = ctx_add ch in
    ch

  let n (s, _ch) =
    s

  let sum_out (n1, ch) (n2, _ch) = 
    play @@ (n1 +~ n2, ch) 
    
end

