open! Core
open! Import

type t =
  { header : Header.t
  ; capabilities : Requirements.Capabilities.t
  ; extensions : Requirements.Extensions.t
  ; instructions : Instruction.t list
  }

let create instructions ~header ~capabilities ~extensions =
  { header; capabilities; extensions; instructions }
;;

let compile { header; capabilities; extensions; instructions } =
  [ Header.compile ~instructions_for_validation:instructions header
  ; Requirements.Capabilities.compile
      ~instructions_for_validation:instructions
      capabilities
  ; Requirements.Extensions.compile ~instructions_for_validation:instructions extensions
  ]
  |> Or_error.all
  |> Or_error.map ~f:List.concat
;;
