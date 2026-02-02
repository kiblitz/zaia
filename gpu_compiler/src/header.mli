open! Core
open! Import

type t

val create
  :  ?generator_id:int32
  -> ?spirv_version:Version.t
  -> id_bound:int32
  -> unit
  -> t

val compile
  :  ?instructions_for_validation:Spirv_instruction.t list
  -> t
  -> int32 list Or_error.t
