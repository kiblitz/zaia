open! Core
open! Import

val create_raw_array : Code.t -> int Ctypes_static.carray Or_error.t
val compile : Code.t -> Vk.Types.Shader_module_create_info.t Or_error.t
