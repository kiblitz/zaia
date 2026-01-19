open! Core
open! Import

module Version : sig
  type t =
    { major : Int32.t
    ; minor : Int32.t
    }
end

type t

val create
  :  ?generator_id:Int32.t
  -> ?spirv_version:Version.t
  -> id_bound:Int32.t
  -> unit
  -> t

val to_repr_list : t -> Int32.t list
