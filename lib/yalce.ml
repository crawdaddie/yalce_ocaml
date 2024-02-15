open Ctypes
open Foreign

type audio_ctx = unit ptr
let audio_ctx : audio_ctx typ = ptr void

type node = unit ptr
let node : node typ = ptr void

let start_audio = foreign "start_audio" (void @-> (returning void))
let get_audio_ctx = foreign "get_audio_ctx" (void @-> (returning audio_ctx)) 
let sq = foreign "sq" (double @-> (returning node))
let lfnoise = foreign "lfnoise" (double @-> double @-> double @-> (returning node))
let add = foreign "ctx_add" (node @-> (returning node))
let pipe_output = foreign "pipe_output" (node @-> node @-> (returning void))
let play = foreign "add_to_dac" (node @-> (returning void))
