open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Value = struct
  type t = int32

  let t_of_yojson = function
    | `Int i -> Int32.of_int_exn i
    | `String s -> Int32.of_string s
    | json_field ->
      let json_field = Yojson.Safe.to_string json_field in
      raise_s
        [%message
          "Expected either [Int] or [String] when parsing a value" (json_field : string)]
  ;;

  let yojson_of_t v = `Int (Int32.to_int_exn v)
end

module Version = struct
  module T = struct
    type t =
      { major : int32
      ; minor : int32
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_string { major; minor } = [%string "%{major#Int32}.%{minor#Int32}"]

  module Option = struct
    module T = struct
      type nonrec t = t option [@@deriving compare, sexp_of]
    end

    include T
    include Comparable.Make_plain (T)

    let t_of_yojson json_field =
      let error () =
        let json_field = Yojson.Safe.to_string json_field in
        raise_s
          [%message "Expected either [None] or [<major>.<minor>]" (json_field : string)]
      in
      match json_field with
      | `Null | `String "None" -> None
      | `String s ->
        (match String.split s ~on:'.' |> List.map ~f:Int32.of_string_opt with
         | [ Some major; Some minor ] -> Some { major; minor }
         | _ -> error ())
      | _ -> error ()
    ;;

    let yojson_of_t = function
      | None -> `Null
      | Some t -> `String (to_string t)
    ;;
  end
end

module Instruction_printing_class = struct
  type t =
    { tag : string
    ; heading : string option [@default None]
    }
  [@@deriving fields ~getters, yojson]
end

module Operand = struct
  module Quantifier = struct
    type t =
      | Star
      | Plus
      | Question

    let t_of_yojson json_field =
      match json_field with
      | `String "*" -> Star
      | `String "+" -> Plus
      | `String "?" -> Question
      | json_field ->
        let json_field = Yojson.Safe.to_string json_field in
        raise_s [%message "Expected one of [*], [+], [?]" (json_field : string)]
    ;;

    let yojson_of_t = function
      | Star -> `String "*"
      | Plus -> `String "+"
      | Question -> `String "?"
    ;;
  end

  type t =
    { kind : string
    ; name : string option [@default None]
    ; quantifier : Quantifier.t option [@default None]
    }
  [@@deriving fields ~getters, yojson]
end

module Instruction = struct
  type t =
    { opname : string
    ; class_ : string [@key "class"]
    ; opcode : int32
    ; version : Version.Option.t [@default None]
    ; operands : Operand.t list [@default []]
    ; capabilities : string list [@default []]
    ; last_version : Version.Option.t [@key "lastVersion"] [@default None]
    ; extensions : string list [@default []]
    ; aliases : string list [@default []]
    ; provisional : bool [@default false]
    }
  [@@deriving fields ~getters, yojson]
end

module Operand_kind = struct
  module Enumerant = struct
    type t =
      { enumerant : string
      ; value : Value.t
      ; capabilities : string list [@default []]
      ; parameters : Operand.t list [@default []]
      ; version : Version.Option.t [@default None]
      ; aliases : string list [@default []]
      ; extensions : string list [@default []]
      ; provisional : bool [@default false]
      ; last_version : Version.Option.t [@key "lastVersion"] [@default None]
      }
    [@@deriving fields ~getters, yojson]
  end

  type t =
    { category : string
    ; kind : string
    ; enumerants : Enumerant.t list [@default []]
    ; doc : string option [@default None]
    ; bases : string list [@default []]
    }
  [@@deriving fields ~getters, yojson]
end

type t =
  { copyright : string list
  ; magic_number : string
  ; major_version : int
  ; minor_version : int
  ; revision : int
  ; instruction_printing_class : Instruction_printing_class.t list
  ; instructions : Instruction.t list
  ; operand_kinds : Operand_kind.t list
  }
[@@deriving fields ~getters, yojson]
