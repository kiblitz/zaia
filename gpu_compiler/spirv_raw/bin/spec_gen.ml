open! Core
open Import

let loc = Ppxlib.Location.none

module Common = struct
  let longident_of_list list =
    let rec loop (list : string Nonempty_list.t) : Longident.t =
      match list with
      | [ head ] -> Lident head
      | [ head; head2 ] -> Ldot (Lident head2, head)
      | head :: head2 :: tail ->
        let tail = Nonempty_list.create head2 tail in
        Ldot (loop tail, head)
    in
    loop (Nonempty_list.reverse list)
  ;;

  let default_derivers = [ "compare"; "sexp_of" ]

  let ppx_deriving derivers =
    let derivers = List.append default_derivers derivers in
    let payload =
      let derivers = String.concat derivers ~sep:", " in
      Ppxlib.Ast_helper.Str.eval (Ppxlib.Ast_builder.Default.evar derivers ~loc)
    in
    Ppxlib.Ast_builder.Default.attribute
      ~loc
      ~name:{ txt = "deriving"; loc }
      ~payload:(PStr [ payload ])
  ;;

  let type_with_attr_exn ?(here = [%here]) (str_item : Ppxlib.structure_item) ~attr =
    match str_item.pstr_desc with
    | Pstr_type (rec_flag, type_decls) ->
      let type_decls_with_attrs =
        List.map type_decls ~f:(fun td ->
          { td with ptype_attributes = attr :: td.ptype_attributes })
      in
      { str_item with pstr_desc = Pstr_type (rec_flag, type_decls_with_attrs) }
    | _ ->
      raise_s
        [%message
          "[type_with_attr_exn] called on non-type" (here : Source_code_position.t)]
  ;;

  let wrap_module_exn
        ?(here = [%here])
        ?(name = "T")
        ?(extra_body = [])
        (str_item : Ppxlib.structure_item)
    =
    match str_item.pstr_desc with
    | Pstr_module { pmb_name; pmb_expr = { pmod_desc = Pmod_structure items; _ }; _ } ->
      let module_binding =
        Ppxlib.Ast_helper.Str.module_
          (Ppxlib.Ast_helper.Mb.mk
             { txt = Some name; loc }
             (Ppxlib.Ast_helper.Mod.structure items))
      in
      let include_t =
        Ppxlib.Ast_helper.Str.include_
          (Ppxlib.Ast_helper.Incl.mk
             (Ppxlib.Ast_helper.Mod.ident { txt = Lident "T"; loc }))
      in
      Ppxlib.Ast_helper.Str.module_
        (Ppxlib.Ast_helper.Mb.mk
           pmb_name
           (Ppxlib.Ast_helper.Mod.structure ([ module_binding; include_t ] @ extra_body)))
    | _ ->
      raise_s
        [%message "[wrap_module_exn] called on non-type" (here : Source_code_position.t)]
  ;;

  let type_t ?attr ?manifest kind =
    let type_decl =
      Ppxlib.Ast_builder.Default.type_declaration
        ~loc
        ~name:{ txt = "t"; loc }
        ~params:[]
        ~cstrs:[]
        ~private_:Ppxlib.Public
        ~manifest
        ~kind
    in
    match attr with
    | None -> type_decl
    | Some attr -> { type_decl with ptype_attributes = [ attr ] }
  ;;

  let enum_t ?args_of_branch branches ~name_of_branch =
    let constructors =
      let%map.List branch = branches in
      let name = branch |> name_of_branch |> Util.machinize in
      let args =
        match args_of_branch with
        | None -> Ppxlib.Pcstr_tuple []
        | Some args_of_branch -> args_of_branch branch
      in
      Ppxlib.Ast_builder.Default.constructor_declaration
        ~loc
        ~name:{ txt = name; loc }
        ~args
        ~res:None
    in
    let type_decl =
      let attr =
        match args_of_branch with
        | None -> ppx_deriving [ "enumerate" ] |> Some
        | Some _ -> None
      in
      type_t (Ppxlib.Ptype_variant constructors) ?attr
    in
    Ppxlib.Ast_helper.Str.type_ Ppxlib.Recursive [ type_decl ]
  ;;

  let any_requirements_fn
        ?(requirement_longident : Longident.t option)
        ?(has_payload = Fn.const false)
        fn_name
        ~(data_by_name : 'a String.Map.t)
        ~(requirements :
           [ `Requirements_of_data of 'a -> string list
           | `Expression_of_data of 'a -> Ppxlib.expression
           ])
    =
    let cases =
      data_by_name
      |> Map.mapi ~f:(fun ~key:name ~data ->
        let extensions =
          match requirements with
          | `Expression_of_data f -> f data
          | `Requirements_of_data f ->
            (let%map.List requirement = f data in
             let txt : Longident.t =
               match requirement_longident with
               | None -> Lident (Util.machinize requirement)
               | Some requirement_long_ident ->
                 Ldot (requirement_long_ident, Util.machinize requirement)
             in
             Ppxlib.Ast_helper.Exp.construct { txt; loc } None)
            |> Ppxlib.Ast_builder.Default.elist ~loc
        in
        let extensions_expr =
          let set_prefix =
            match requirement_longident with
            | Some requirement_longident ->
              Longident.flatten requirement_longident
              |> String.concat ~sep:"."
              |> Fn.flip String.append "."
            | None -> ""
          in
          [%expr
            [%e extensions]
            |> [%e
                 Ppxlib.Ast_builder.Default.evar ~loc [%string "%{set_prefix}Set.of_list"]]]
        in
        Ppxlib.Ast_helper.Exp.case
          (Ppxlib.Ast_helper.Pat.construct
             { txt = Lident name; loc }
             (if not (has_payload data)
              then None
              else Ppxlib.Ast_builder.Default.ppat_any ~loc |> Some))
          extensions_expr)
      |> Map.data
    in
    let body =
      match cases with
      | [] -> [%expr []]
      | _ :: _ -> Ppxlib.Ast_helper.Exp.function_ cases
    in
    let fn_name = Ppxlib.Ast_builder.Default.pvar fn_name ~loc in
    [%stri let [%p fn_name] = [%e body]]
  ;;

  let option_to_expression t ~expr_maker =
    match t with
    | Some value -> [%expr Some [%e expr_maker ~loc value]]
    | None -> [%expr None]
  ;;

  module Requirements = struct
    let structure
          ?(capabilities_longident_prefix : Longident.t Option.t)
          ?has_payload
          data_by_name
          ~extensions_of_data
          ~capabilities_of_data
          ~version_of_data
          ~last_version_of_data
      =
      let any_required_extension_fn =
        any_requirements_fn
          "any_required_extension"
          ?has_payload
          ~requirement_longident:(Ldot (Lident "Requirements", "Extension"))
          ~data_by_name
          ~requirements:(`Requirements_of_data extensions_of_data)
      in
      let any_required_capability_fn =
        any_requirements_fn
          "any_required_capability"
          ?has_payload
          ?requirement_longident:capabilities_longident_prefix
          ~data_by_name
          ~requirements:(`Requirements_of_data capabilities_of_data)
      in
      let any_required_version_fn =
        let requirements =
          let f data =
            let version_to_expression =
              Option.map ~f:(Grammar.Version.to_string >> Util.machinize)
              >> option_to_expression ~expr_maker:Ppxlib.Ast_builder.Default.evar
            in
            let required = version_to_expression (version_of_data data) in
            let last = version_to_expression (last_version_of_data data) in
            [%expr
              Requirements.Version.valid_versions ~required:[%e required] ~last:[%e last]]
          in
          `Expression_of_data f
        in
        any_requirements_fn
          "any_required_version"
          ?has_payload
          ~requirement_longident:(Ldot (Lident "Requirements", "Version"))
          ~data_by_name
          ~requirements
      in
      [ any_required_version_fn; any_required_extension_fn; any_required_capability_fn ]
    ;;
  end
end

module Requirements = struct
  let generate
        ?(extra_body = Fn.const [])
        (grammar : Grammar.t)
        ~name
        ~module_
        ~of_instruction
        ~of_enumerant
        ~m_to_string
    =
    let all =
      let from_instructions =
        grammar.instructions |> List.concat_map ~f:of_instruction |> Set.of_list module_
      in
      let from_enumerants =
        (let%bind.List operand_kinds = grammar.operand_kinds in
         let%bind.List enumerant = operand_kinds.enumerants in
         of_enumerant enumerant)
        |> Set.of_list module_
      in
      Set.union from_instructions from_enumerants
    in
    let module_binding =
      let type_ = Common.enum_t (Set.to_list all) ~name_of_branch:m_to_string in
      let module_expr = Ppxlib.Ast_helper.Mod.structure [ type_ ] in
      Ppxlib.Ast_helper.Mb.mk { txt = Some name; loc } module_expr
    in
    let extra_body =
      let include_comparable = [%stri include Comparable.Make_plain (T)] in
      include_comparable :: extra_body all
    in
    Ppxlib.Ast_helper.Str.module_ module_binding |> Common.wrap_module_exn ~extra_body
  ;;

  module Version = struct
    let generate =
      let extra_body all =
        let major_and_minor_fn =
          let cases getter =
            let%map.List (version : Grammar.Version.t) = Set.to_list all in
            Ppxlib.Ast_helper.Exp.case
              (Ppxlib.Ast_helper.Pat.construct
                 { txt = Lident (Grammar.Version.to_string version |> Util.machinize)
                 ; loc
                 }
                 None)
              (Ppxlib.Ast_builder.Default.eint32 (getter version) ~loc)
          in
          let major_fn =
            let body = Ppxlib.Ast_helper.Exp.function_ (cases Grammar.Version.major) in
            [%stri let major = [%e body]]
          in
          let minor_fn =
            let body = Ppxlib.Ast_helper.Exp.function_ (cases Grammar.Version.minor) in
            [%stri let minor = [%e body]]
          in
          [ major_fn; minor_fn ]
        in
        let value_fn =
          [%stri
            let value t =
              let major_value = Int32.shift_left (major t) 16 in
              let minor_value = Int32.shift_left (minor t) 8 in
              Int32.bit_or minor_value major_value
            ;;]
        in
        let valid_versions_fn =
          [%stri
            let valid_versions ~required ~last =
              List.filter all ~f:(fun t ->
                Option.value_map required ~default:true ~f:(fun required -> t >= required)
                && Option.value_map last ~default:true ~f:(fun last -> t <= last))
            ;;]
        in
        major_and_minor_fn @ [ value_fn; valid_versions_fn ]
      in
      generate
        ~name:"Version"
        ~module_:(module Grammar.Version)
        ~of_instruction:(Grammar.Instruction.version >> Option.to_list)
        ~of_enumerant:(Grammar.Operand_kind.Enumerant.version >> Option.to_list)
        ~m_to_string:Grammar.Version.to_string
        ~extra_body
    ;;
  end

  module Extension = struct
    let generate =
      generate
        ~name:"Extension"
        ~module_:(module String)
        ~of_instruction:Grammar.Instruction.extensions
        ~of_enumerant:Grammar.Operand_kind.Enumerant.extensions
        ~m_to_string:Fn.id
    ;;
  end

  let generate (grammar : Grammar.t) =
    let module_expr =
      Ppxlib.Ast_helper.Mod.structure
        [ Version.generate grammar; Extension.generate grammar ]
    in
    [%stri module Requirements = [%m module_expr]]
  ;;
end

module Instruction_printing_class = struct
  let generate (grammar : Grammar.t) =
    let%map.Or_error heading_by_instruction_printing_class_tag =
      grammar.instruction_printing_class
      |> String.Map.of_list_with_key_or_error
           ~get_key:(Grammar.Instruction_printing_class.tag >> Util.machinize)
      |> Or_error.map ~f:(Map.map ~f:Grammar.Instruction_printing_class.heading)
    in
    let type_ =
      Common.enum_t
        (Map.keys heading_by_instruction_printing_class_tag)
        ~name_of_branch:Fn.id
    in
    let heading_fn =
      let cases =
        heading_by_instruction_printing_class_tag
        |> Map.mapi ~f:(fun ~key:tag ~data:heading ->
          Ppxlib.Ast_helper.Exp.case
            (Ppxlib.Ast_helper.Pat.construct { txt = Lident tag; loc } None)
            (Common.option_to_expression
               heading
               ~expr_maker:Ppxlib.Ast_builder.Default.estring))
        |> Map.data
      in
      let body = Ppxlib.Ast_helper.Exp.function_ cases in
      [%stri let heading = [%e body]]
    in
    let module_expr = Ppxlib.Ast_helper.Mod.structure [ type_; heading_fn ] in
    [%stri module Instruction_printing_class = [%m module_expr]]
  ;;
end

module Operand_kinds = struct
  module Payload = struct
    let generate (grammar : Grammar.t) =
      let%bind.Or_error operand_kind_by_name =
        String.Map.of_list_with_key_or_error
          grammar.operand_kinds
          ~get_key:(Grammar.Operand_kind.kind >> Util.machinize)
      in
      let%map.Or_error payload_modules =
        let operand_kind_module_by_name =
          Map.mapi operand_kind_by_name ~f:(fun ~key:name ~data:operand_kind ->
            let is_capability_module = [%equal: string] name "Capability" in
            let%map.Or_error enumerant_by_name =
              String.Map.of_list_with_key_or_error
                operand_kind.enumerants
                ~get_key:(Grammar.Operand_kind.Enumerant.enumerant >> Util.machinize)
            in
            let type_, opcode_fn =
              match operand_kind.category, name with
              | "ValueEnum", _ | "BitEnum", _ ->
                let opcode_fn =
                  let cases =
                    enumerant_by_name
                    |> Map.mapi ~f:(fun ~key:name ~data:enumerant ->
                      Ppxlib.Ast_helper.Exp.case
                        (Ppxlib.Ast_helper.Pat.construct { txt = Lident name; loc } None)
                        [%expr
                          [ [%e Ppxlib.Ast_builder.Default.eint32 enumerant.value ~loc] ]])
                    |> Map.data
                  in
                  let body = Ppxlib.Ast_helper.Exp.function_ cases in
                  [%stri let value = [%e body]]
                in
                ( Common.enum_t (Map.keys enumerant_by_name) ~name_of_branch:Fn.id
                , opcode_fn )
              | category, class_ ->
                (match category, class_ with
                 | "Id", _
                 | "Literal", "Literalinteger"
                 | "Literal", "Literalspecconstantopinteger"
                 | "Literal", "Literalextinstinteger" ->
                   [%stri type t = int32], [%stri let value t = [ t ]]
                 | "Literal", "Literalfloat" ->
                   ( [%stri
                       type t =
                         | F32 of float
                         | F64 of float]
                   , [%stri
                       let value = function
                         | F32 float -> [ Int32.bits_of_float float ]
                         | F64 float ->
                           let int64 = Int64.bits_of_float float in
                           let low = Int64.to_int32_trunc int64 in
                           let high =
                             Int64.to_int32_trunc (Int64.shift_right_logical int64 32)
                           in
                           [ low; high ]
                       ;;] )
                 | "Literal", "Literalcontextdependentnumber" ->
                   ( [%stri
                       type t =
                         | I32 of int32
                         | I64 of int64]
                   , [%stri
                       let value = function
                         | I32 int32 -> [ int32 ]
                         | I64 int64 ->
                           let low = Int64.to_int32_trunc int64 in
                           let high =
                             Int64.to_int32_trunc (Int64.shift_right_logical int64 32)
                           in
                           [ low; high ]
                       ;;] )
                 | "Literal", "Literalstring" ->
                   ( [%stri type t = string]
                   , [%stri
                       let value t =
                         let bytes = Bytes.of_string (t ^ "\x00") in
                         let int32_size = 4 in
                         let output_size =
                           (Bytes.length bytes + (int32_size - 1)) / int32_size
                         in
                         List.init output_size ~f:(fun idx ->
                           EndianBytes.LittleEndian.get_int32 bytes (idx * 4))
                       ;;] )
                 | "Composite", _ ->
                   let type_ =
                     let tuple =
                       let%map.List base = operand_kind.bases in
                       Ppxlib.Ast_helper.Typ.mk
                         (Ptyp_constr
                            ({ txt = Ldot (Lident (Util.machinize base), "t"); loc }, []))
                     in
                     Common.type_t
                       Ptype_abstract
                       ~manifest:(Ppxlib.Ast_helper.Typ.tuple tuple)
                     |> List.singleton
                     |> Ppxlib.Ast_helper.Str.type_ Ppxlib.Recursive
                   in
                   let value =
                     let var_names =
                       List.mapi operand_kind.bases ~f:(fun idx base ->
                         let name = Util.machinize base ~lowercase:true in
                         [%string "%{name}%{idx#Int}"])
                     in
                     let tuple =
                       let%map.List var_name = var_names in
                       Ppxlib.Ast_helper.Pat.var { txt = var_name; loc } ~loc
                     in
                     let expr =
                       let%map.List var_name = var_names in
                       Ppxlib.Ast_helper.Exp.ident { txt = Lident var_name; loc } ~loc
                     in
                     [%stri
                       let value [%p Ppxlib.Ast_helper.Pat.tuple tuple ~loc] =
                         [%e Ppxlib.Ast_builder.Default.elist expr ~loc]
                       ;;]
                   in
                   type_, value
                 | category, class_ ->
                   raise_s
                     [%message
                       "Unmatched class/category when defining [type t]"
                         (category : string)
                         (class_ : string)])
                |> Tuple2.map_fst
                     ~f:(Common.type_with_attr_exn ~attr:(Common.ppx_deriving []))
            in
            let category_const =
              let value =
                Ppxlib.Ast_helper.Exp.construct
                  { txt = Ldot (Lident "Category", Util.machinize operand_kind.category)
                  ; loc
                  }
                  None
              in
              [%stri let category = [%e value]]
            in
            let doc_const =
              let expr =
                Common.option_to_expression
                  operand_kind.doc
                  ~expr_maker:Ppxlib.Ast_builder.Default.estring
              in
              [%stri let doc = [%e expr]]
            in
            let requirement_fns =
              Common.Requirements.structure
                enumerant_by_name
                ?capabilities_longident_prefix:
                  (if is_capability_module then None else Lident "Capability" |> Some)
                ~extensions_of_data:Grammar.Operand_kind.Enumerant.extensions
                ~capabilities_of_data:Grammar.Operand_kind.Enumerant.capabilities
                ~version_of_data:Grammar.Operand_kind.Enumerant.version
                ~last_version_of_data:Grammar.Operand_kind.Enumerant.last_version
            in
            let module_body =
              [ category_const; doc_const; opcode_fn ] @ requirement_fns
            in
            let module_ body =
              let module_expr = Ppxlib.Ast_helper.Mod.structure (type_ :: body) in
              let module_binding =
                Ppxlib.Ast_helper.Mb.mk { txt = Some name; loc } module_expr
              in
              Ppxlib.Ast_helper.Str.module_ module_binding
            in
            if not is_capability_module
            then module_ module_body
            else (
              let extra_body =
                let include_comparable = [%stri include Comparable.Make_plain (T)] in
                include_comparable :: module_body
              in
              Common.wrap_module_exn (module_ []) ~extra_body))
        in
        operand_kind_module_by_name
        |>
        (* We need to define [Capability] first since other modules depend on it. *)
        Map.partitioni_tf ~f:(fun ~key:name ~data:_ -> [%equal: string] name "Capability")
        |> Tuple2.map ~f:Map.data
        |> Tuple2.uncurry List.append
        |> Or_error.all
      in
      let module_expr = Ppxlib.Ast_helper.Mod.structure payload_modules in
      [%stri module Payload = [%m module_expr]]
    ;;
  end

  let generate (grammar : Grammar.t) =
    let categories =
      grammar.operand_kinds
      |> List.map ~f:(Grammar.Operand_kind.category >> Util.machinize)
      |> String.Set.of_list
    in
    let category_module =
      let type_ = Common.enum_t (Set.to_list categories) ~name_of_branch:Fn.id in
      let module_expr = Ppxlib.Ast_helper.Mod.structure [ type_ ] in
      [%stri module Category = [%m module_expr]]
    in
    let%map.Or_error payload_module = Payload.generate grammar in
    let module_expr =
      Ppxlib.Ast_helper.Mod.structure [ category_module; payload_module ]
    in
    [%stri module Operand_kind = [%m module_expr]]
  ;;
end

module Instructions = struct
  let generate (grammar : Grammar.t) =
    let%map.Or_error instruction_by_name_by_class =
      let%map.Or_error instruction_by_name =
        String.Map.of_list_with_key_or_error
          grammar.instructions
          ~get_key:(Grammar.Instruction.opname >> Util.machinize)
      in
      instruction_by_name
      |> Map.map ~f:(fun instruction ->
        String.Map.singleton (Util.machinize instruction.class_) instruction)
      |> Map.transpose_keys (module String)
    in
    let payload_longident list =
      Common.longident_of_list (Nonempty_list.append [ "Operand_kind"; "Payload" ] list)
    in
    let instruction_printing_class_module instruction_printing_class ~instruction_by_name =
      let has_payload (instruction : Grammar.Instruction.t) =
        not (List.is_empty instruction.operands)
      in
      let record_field_of_operand (operand : Grammar.Operand.t) ~name_cache =
        let name =
          Option.value operand.name ~default:operand.kind
          |> Util.machinize ~lowercase:true
        in
        let count =
          Hashtbl.update_and_return
            name_cache
            name
            ~f:(Option.value ~default:0 >> Int.succ)
        in
        if count = 1 then name else [%string "%{name}%{count#Int}"]
      in
      let operand_kind_longident (operand : Grammar.Operand.t) subpath =
        payload_longident (Util.machinize operand.kind :: subpath)
      in
      let type_ =
        let args_of_branch (instruction : Grammar.Instruction.t) =
          let record_fields =
            let name_cache = String.Table.create () in
            let%map.List operand = instruction.operands in
            let name = record_field_of_operand operand ~name_cache in
            let type_ =
              let base_type =
                Ppxlib.Ast_helper.Typ.constr
                  { txt = operand_kind_longident operand [ "t" ]; loc }
                  []
              in
              match operand.quantifier with
              | None -> base_type
              | Some Star ->
                Ppxlib.Ast_helper.Typ.constr { txt = Lident "list"; loc } [ base_type ]
              | Some Plus ->
                Ppxlib.Ast_helper.Typ.constr
                  { txt = Ldot (Lident "Nonempty_list", "t"); loc }
                  [ base_type ]
              | Some Question ->
                Ppxlib.Ast_helper.Typ.constr { txt = Lident "option"; loc } [ base_type ]
            in
            Ppxlib.Ast_helper.Type.field { txt = name; loc } type_
          in
          if has_payload instruction
          then Ppxlib.Pcstr_record record_fields
          else Ppxlib.Pcstr_tuple []
        in
        Common.enum_t
          (Map.data instruction_by_name)
          ~args_of_branch
          ~name_of_branch:Grammar.Instruction.opname
      in
      let provisional_fn =
        let cases =
          instruction_by_name
          |> Map.mapi ~f:(fun ~key:name ~data:instruction ->
            Ppxlib.Ast_helper.Exp.case
              (Ppxlib.Ast_helper.Pat.construct
                 { txt = Lident name; loc }
                 (if not (has_payload instruction)
                  then None
                  else Ppxlib.Ast_builder.Default.ppat_any ~loc |> Some))
              [%expr [%e Ppxlib.Ast_builder.Default.ebool instruction.provisional ~loc]])
          |> Map.data
        in
        let body = Ppxlib.Ast_helper.Exp.function_ cases in
        [%stri let provisional = [%e body]]
      in
      let opcode_fn =
        let cases =
          instruction_by_name
          |> Map.mapi ~f:(fun ~key:name ~data:instruction ->
            if not (has_payload instruction)
            then
              Ppxlib.Ast_helper.Exp.case
                (Ppxlib.Ast_helper.Pat.construct { txt = Lident name; loc } None)
                [%expr
                  let heading =
                    let size = Int.shift_left 1 16 |> Int32.of_int_trunc in
                    Int32.bit_or
                      size
                      [%e Ppxlib.Ast_builder.Default.eint32 instruction.opcode ~loc]
                  in
                  [ heading ]]
            else (
              let expr =
                let record_fields =
                  let name_cache = String.Table.create () in
                  let%map.List operand = instruction.operands in
                  let value_fn_expr =
                    let expr =
                      operand_kind_longident operand [ "value" ]
                      |> Longident.flatten
                      |> String.concat ~sep:"."
                    in
                    Ppxlib.Ast_builder.Default.evar expr ~loc
                  in
                  let base_expr =
                    let field = record_field_of_operand operand ~name_cache in
                    Ppxlib.Ast_builder.Default.evar [%string "t.%{field}"] ~loc
                  in
                  match operand.quantifier with
                  | None -> [%expr [%e value_fn_expr] [%e base_expr]]
                  | Some Star ->
                    [%expr List.concat_map [%e base_expr] ~f:[%e value_fn_expr]]
                  | Some Plus ->
                    [%expr
                      [%e base_expr]
                      |> Nonempty_list.concat_map ~f:[%e value_fn_expr]
                      |> Nonempty_list.to_list]
                  | Some Question ->
                    [%expr
                      [%e base_expr]
                      |> Option.map ~f:[%e value_fn_expr]
                      |> Option.value ~default:[]]
                in
                [%expr
                  let payload =
                    List.concat [%e Ppxlib.Ast_builder.Default.elist record_fields ~loc]
                  in
                  let heading =
                    let size =
                      Int.shift_left (1 + List.length payload) 16 |> Int32.of_int_trunc
                    in
                    Int32.bit_or
                      size
                      [%e Ppxlib.Ast_builder.Default.eint32 instruction.opcode ~loc]
                  in
                  heading :: payload]
              in
              Ppxlib.Ast_helper.Exp.case
                (Ppxlib.Ast_helper.Pat.construct
                   { txt = Lident name; loc }
                   (Ppxlib.Ast_helper.Pat.var { txt = "t"; loc } |> Some))
                expr))
          |> Map.data
        in
        let body = Ppxlib.Ast_helper.Exp.function_ cases in
        [%stri let value = [%e body]]
      in
      let requirement_fns =
        Common.Requirements.structure
          instruction_by_name
          ?capabilities_longident_prefix:(payload_longident [ "Capability" ] |> Some)
          ~has_payload
          ~extensions_of_data:Grammar.Instruction.extensions
          ~capabilities_of_data:Grammar.Instruction.capabilities
          ~version_of_data:Grammar.Instruction.version
          ~last_version_of_data:Grammar.Instruction.last_version
      in
      let module_expr =
        Ppxlib.Ast_helper.Mod.structure
          ([ type_; provisional_fn; opcode_fn ] @ requirement_fns)
      in
      Ppxlib.Ast_helper.Str.module_
        (Ppxlib.Ast_helper.Mb.mk
           { txt = Some instruction_printing_class; loc }
           module_expr)
    in
    let module_expr =
      let type_ =
        let args_of_branch instruction_printing_class =
          Ppxlib.Pcstr_tuple
            [ Ppxlib.Ast_helper.Typ.mk
                (Ptyp_constr
                   ({ txt = Ldot (Lident instruction_printing_class, "t"); loc }, []))
            ]
        in
        Common.enum_t
          ~args_of_branch
          (Map.keys instruction_by_name_by_class)
          ~name_of_branch:Fn.id
      in
      let fns =
        let fn fn_name =
          let cases =
            instruction_by_name_by_class
            |> Map.mapi ~f:(fun ~key:class_ ~data:_ ->
              let value_fn_expr =
                let expr =
                  Common.longident_of_list Nonempty_list.[ class_; fn_name ]
                  |> Longident.flatten
                  |> String.concat ~sep:"."
                in
                [%expr [%e Ppxlib.Ast_builder.Default.evar expr ~loc] t]
              in
              Ppxlib.Ast_helper.Exp.case
                (Ppxlib.Ast_helper.Pat.construct
                   { txt = Lident class_; loc }
                   (Ppxlib.Ast_helper.Pat.var { txt = "t"; loc } |> Some))
                [%expr [%e value_fn_expr]])
            |> Map.data
          in
          let fn_name = Ppxlib.Ast_builder.Default.pvar fn_name ~loc in
          let body = Ppxlib.Ast_helper.Exp.function_ cases in
          [%stri let [%p fn_name] = [%e body]]
        in
        [ "provisional"; "value"; "any_required_version"; "any_required_capability" ]
        |> List.map ~f:fn
      in
      let instruction_printing_class_modules =
        instruction_by_name_by_class
        |> Map.mapi ~f:(fun ~key:instruction_printing_class ~data:instruction_by_name ->
          instruction_printing_class_module
            instruction_printing_class
            ~instruction_by_name)
        |> Map.data
      in
      Ppxlib.Ast_helper.Mod.structure (instruction_printing_class_modules @ (type_ :: fns))
    in
    [%stri module Instruction = [%m module_expr]]
  ;;
end

let generate (grammar : Grammar.t) =
  let requirements = Requirements.generate grammar in
  let%bind.Or_error instruction_printing_class =
    Instruction_printing_class.generate grammar
  and operand_kinds = Operand_kinds.generate grammar
  and instructions = Instructions.generate grammar in
  let unused_warning = [%stri [@@@warning "-32"]] in
  let open_core = [%stri open Core] in
  Ok
    [ unused_warning
    ; open_core
    ; requirements
    ; instruction_printing_class
    ; operand_kinds
    ; instructions
    ]
;;
