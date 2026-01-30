open! Core
open Import

module Capabilities : sig
  type t

  val create : Capability.t list -> t

  val compile
    :  ?instructions_for_validation:Instruction.t list
    -> t
    -> int32 list Or_error.t
end

module Extensions : sig
  type t

  val create : Extension.t list -> t

  val compile
    :  ?instructions_for_validation:Instruction.t list
    -> t
    -> int32 list Or_error.t
end
