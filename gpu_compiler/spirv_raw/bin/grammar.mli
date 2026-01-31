open! Core

module Value : sig
  type t = int32 [@@deriving yojson]
end

module Version : sig
  type t =
    { major : int32
    ; minor : int32
    }
  [@@deriving fields ~getters, to_string]

  include Comparable.S_plain with type t := t

  module Option : sig
    type nonrec t = t option [@@deriving yojson]

    include Comparable.S_plain with type t := t
  end
end

module Instruction_printing_class : sig
  type t =
    { tag : string
    ; heading : string option
    }
  [@@deriving fields ~getters, yojson]
end

module Operand : sig
  module Quantifier : sig
    type t =
      | Star
      | Plus
      | Question
    [@@deriving yojson]
  end

  type t =
    { kind : string
    ; name : string option
    ; quantifier : Quantifier.t option
    }
  [@@deriving fields ~getters, yojson]
end

module Instruction : sig
  type t =
    { opname : string
    ; class_ : string
    ; opcode : int32
    ; version : Version.Option.t
    ; operands : Operand.t list
    ; capabilities : string Nonempty_list.t option
    ; last_version : Version.Option.t
    ; extensions : string Nonempty_list.t option
    ; aliases : string list
    ; provisional : bool
    }
  [@@deriving fields ~getters, yojson]
end

module Operand_kind : sig
  module Enumerant : sig
    type t =
      { enumerant : string
      ; value : Value.t
      ; capabilities : string Nonempty_list.t option
      ; parameters : Operand.t list
      ; version : Version.Option.t
      ; aliases : string list
      ; extensions : string Nonempty_list.t option
      ; provisional : bool
      ; last_version : Version.Option.t
      }
    [@@deriving fields ~getters, yojson]
  end

  type t =
    { category : string
    ; kind : string
    ; enumerants : Enumerant.t list
    ; doc : string option
    ; bases : string list
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
