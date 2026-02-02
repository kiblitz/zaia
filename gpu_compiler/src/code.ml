open! Core
open! Import

type t =
  { header : Header.t
  ; capabilities : Requirements.Capabilities.t
  ; extensions : Requirements.Extensions.t
  ; instructions : Instruction.List.t
  }

let create instructions ~header ~capabilities ~extensions =
  { header; capabilities; extensions; instructions }
;;

let compile { header; capabilities; extensions; instructions } =
  let spirv_instructions = Instruction.List.compile_to_spirv instructions in
  [ Header.compile ~instructions_for_validation:spirv_instructions header
  ; Requirements.Capabilities.compile
      ~instructions_for_validation:spirv_instructions
      capabilities
  ; Requirements.Extensions.compile
      ~instructions_for_validation:spirv_instructions
      extensions
  ; List.concat_map spirv_instructions ~f:Spirv_instruction.value |> Or_error.return
  ]
  |> Or_error.all
  |> Or_error.map ~f:List.concat
;;
