open! Core
open! Import

val create_raw_array : header:Header.t -> int Ctypes_static.carray
val compile : header:Header.t -> Vk.Types.Shader_module_create_info.t
