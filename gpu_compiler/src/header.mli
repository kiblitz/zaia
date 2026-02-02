open! Core
open! Import

type t

val create : ?generator_id:int32 -> ?spirv_version:Version.t -> unit -> t

val compile
  :  ?validate_requirements:bool
  -> t
  -> instructions:Spirv_instruction.t list
  -> int32 list Or_error.t
