open Ctypes
open Foreign

let start_audio = foreign "setup_audio" (void @-> (returning void))
let midi_setup = foreign "midi_setup" (void @-> (returning void))
let version = foreign "_caml_get_version" (void @-> (returning int))

