open! Core

module Arg = struct
  let spec_file =
    let info =
      Cmdliner.Arg.info
        [ "spec-file" ]
        ~docv:"FILE"
        ~doc:"SPIR-V header file to parse for generating bindings"
    in
    Cmdliner.(Arg.required (Arg.opt (Arg.some Arg.file) None info))
  ;;

  let output_file =
    let info =
      Cmdliner.Arg.info
        [ "o"; "output-file" ]
        ~docv:"FILE"
        ~doc:"ML file to output generated bindings to"
    in
    Cmdliner.(Arg.required (Arg.opt (Arg.some Arg.filepath) None info))
  ;;
end

let command =
  let info =
    Cmdliner.Cmd.info
      ~doc:"cli tool for generating SPIR-V spec .ml file"
      "spirv_spec_gen_bin"
  in
  let main spec_file output_file =
    let structure =
      Yojson.Safe.from_file spec_file
      |> [%of_yojson: Grammar.t]
      |> Spec_gen.generate
      |> ok_exn
    in
    Out_channel.with_file output_file ~f:(fun out_channel ->
      let fmt = Format.formatter_of_out_channel out_channel in
      Ppxlib.Pprintast.structure fmt structure;
      Format.pp_print_flush fmt ())
  in
  Cmdliner.Cmd.v info Cmdliner.Term.(const main $ Arg.spec_file $ Arg.output_file)
;;

let () = Cmdliner.Cmd.eval command |> exit
