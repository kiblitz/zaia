open! Core
open! Import

type t = Spirv of Spirv_instruction.t list

module List = struct
  type nonrec t = t list

  let compile_to_spirv ts =
    List.concat_map ts ~f:(fun t ->
      match t with
      | Spirv instructions -> instructions)
  ;;
end
