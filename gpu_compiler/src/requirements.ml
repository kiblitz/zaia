open! Core
open Import

module Capabilities = struct
  type t = Capability.Set.t [@@deriving sexp_of]

  let create (capabilities : Capability.t list) = Capability.Set.of_list capabilities

  let compile ?instructions_for_validation t =
    let validate =
      match instructions_for_validation with
      | None -> Ok ()
      | Some (instructions : Spirv_instruction.t list) ->
        (let%map.List instruction = instructions in
         if Spirv_instruction.satisfies_capabilities instruction ~capabilities:t
         then Ok ()
         else
           Or_error.error_s
             [%message
               "Missing capability"
                 (t : Capability.Set.t)
                 (instruction : Spirv_instruction.t)])
        |> Or_error.all_unit
        |> Or_error.tag_s ~tag:[%message (t : t)]
    in
    let%map.Or_error () = validate in
    let%bind.List capability = Set.to_list t in
    Spirv_instruction.Mode_setting (Opcapability { capability })
    |> Spirv_instruction.value
  ;;
end

module Extensions = struct
  type t = Extension.Set.t [@@deriving sexp_of]

  let create (extensions : Extension.t list) = Extension.Set.of_list extensions

  let compile ?instructions_for_validation t =
    let validate =
      match instructions_for_validation with
      | None -> Ok ()
      | Some (instructions : Spirv_instruction.t list) ->
        (let%map.List instruction = instructions in
         if Spirv_instruction.satisfies_extensions instruction ~extensions:t
         then Ok ()
         else
           Or_error.error_s
             [%message
               "Missing extension"
                 (t : Extension.Set.t)
                 (instruction : Spirv_instruction.t)])
        |> Or_error.all_unit
        |> Or_error.tag_s ~tag:[%message (t : t)]
    in
    let%map.Or_error () = validate in
    let%bind.List extension = Set.to_list t in
    let name = Extension.to_string extension in
    Spirv_instruction.Extension (Opextension { name }) |> Spirv_instruction.value
  ;;
end
