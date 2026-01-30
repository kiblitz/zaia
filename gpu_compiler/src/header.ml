open! Core
open! Import

let generator_id =
  let khronos_unknown = 0l in
  khronos_unknown
;;

type t =
  { magic_number : int32
  ; spirv_version : Version.t
  ; generator_id : int32
  ; id_bound : int32
  ; reserved : int32
  }

let create ?(generator_id = generator_id) ?(spirv_version : Version.t = V1_3) ~id_bound ()
  =
  let magic_number = 119734787l in
  let reserved = 0l in
  { magic_number; spirv_version; generator_id; id_bound; reserved }
;;

let compile
      ?instructions_for_validation
      { magic_number; spirv_version; generator_id; id_bound; reserved }
  =
  let validate =
    match instructions_for_validation with
    | None -> Ok ()
    | Some (instructions : Instruction.t list) ->
      (let%map.List instruction = instructions in
       let required = Instruction.any_required_version instruction in
       if Set.mem required spirv_version
       then Ok ()
       else
         Or_error.error_s
           [%message
             "Incompatible version"
               (instruction : Instruction.t)
               (required : Version.Set.t)])
      |> Or_error.all_unit
      |> Or_error.tag_s ~tag:[%message (spirv_version : Version.t)]
  in
  let%map.Or_error () = validate in
  [ magic_number; Version.value spirv_version; generator_id; id_bound; reserved ]
;;
