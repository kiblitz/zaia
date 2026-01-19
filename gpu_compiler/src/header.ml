open! Core
open! Import

let generator_id =
  let khronos_unknown = Int32.zero in
  khronos_unknown
;;

module Version = struct
  type t =
    { major : Int32.t
    ; minor : Int32.t
    }

  let default = { major = 1l; minor = 3l }

  let to_repr { major; minor } =
    let minor_repr = Int32.shift_left minor 8 in
    let major_repr = Int32.shift_left major 16 in
    Int32.bit_or minor_repr major_repr
  ;;

  let%expect_test "repr" =
    print_endline (Int32.Hex.to_string (to_repr default));
    [%expect {| 0x10300 |}]
  ;;
end

type t =
  { magic_number : Int32.t
  ; spirv_version : Version.t
  ; generator_id : Int32.t
  ; id_bound : Int32.t
  ; reserved : Int32.t
  }

let create ?(generator_id = generator_id) ?(spirv_version = Version.default) ~id_bound () =
  let magic_number = 119734787l in
  let reserved = Int32.zero in
  { magic_number; spirv_version; generator_id; id_bound; reserved }
;;

let to_repr_list { magic_number; spirv_version; generator_id; id_bound; reserved } =
  [ magic_number; Version.to_repr spirv_version; generator_id; id_bound; reserved ]
;;
