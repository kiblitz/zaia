open! Core
open! Import

type t = Spirv of Spirv_instruction.t list

module List : sig
  type nonrec t = t list

  val compile_to_spirv : t -> Spirv_instruction.t list
end
