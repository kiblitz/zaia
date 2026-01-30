open! Core
open! Import

let create_raw_array code =
  let%map.Or_error compiled = Code.compile code in
  let data = Ctypes.CArray.of_list Ctypes.int32_t compiled in
  let ptr =
    (* TODO soon: I probably have some fundamental misunderstanding of OCaml
       memory representation but I don't understand why the Olivine API takes
       as input an [int CArray.t] rather than an [int32 CArray.t]. This hacky
       converstion is to fit those types. *)
    Ctypes.coerce
      (Ctypes.ptr Ctypes.int32_t)
      (Ctypes.ptr Vk__builtin__types.uint_32_t)
      (Ctypes.CArray.start data)
  in
  Ctypes.CArray.from_ptr ptr (Ctypes.CArray.length data)
;;

let compile code =
  let%map.Or_error data = create_raw_array code in
  Vk.Types.Shader_module_create_info.make
    ~code_size:(Ctypes.CArray.length data |> Unsigned.Size_t.of_int)
    ~code:data
    ()
;;
