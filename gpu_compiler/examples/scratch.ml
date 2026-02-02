open! Core
open Import

let header = Header.create ~id_bound:Int32.zero ()
let capabilities = Requirements.Capabilities.create [ Shader; Matrix; Int64 ]
let extensions = Requirements.Extensions.create [ Spv_khr_storage_buffer_storage_class ]
let instructions = []
let code = Code.create instructions ~header ~capabilities ~extensions
