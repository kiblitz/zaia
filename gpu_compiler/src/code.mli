open! Core
open! Import

type t

val create
  :  Instruction.List.t
  -> header:Header.t
  -> capabilities:Requirements.Capabilities.t
  -> extensions:Requirements.Extensions.t
  -> t

val compile : t -> int32 list Or_error.t
