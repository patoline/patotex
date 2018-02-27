open Latex
open LatexParser
open LatexAst_helper

(** Command-line arguments for the main executable *)
module ArgSpec = struct
  open Arg

  let spec = [ ]
  let file = ref None
  let set_files name =
    file := Some name

  let usage = "patothez [FILE]"

  let parse () =
    try parse spec set_files usage
    with
    | Bad(msg) -> Printf.fprintf stderr "Wrong argument %s\n" msg
    | Help(msg) -> Printf.fprintf stderr "%s\n%s\n" usage msg
end

(** Syntax error pretty-printing *)
let print_syntax_error pos err =
  Location.(print_error Format.err_formatter pos);
  begin
    match err with
    | Syntax_error -> Format.eprintf " syntax error"
    | Unmatched_environment(start, close) ->
        Format.eprintf
          " unmatched environment\n\\begin{%s} was closed with \\end{%s}"
          (txt start) (txt close);
        Location.(print_loc Format.err_formatter start.loc);
        Format.eprintf ": start tag was here"
    | Unterminated_environment(start) ->
        Format.eprintf " missing \\end{%s}\n" (txt start);
        Location.(print_loc Format.err_formatter start.loc);
        Format.eprintf ": start tag was here"
    | Document_environment ->
        Format.eprintf " document content must be enclosed inside \\begin{document} and \\end{document}"
    | Preamble_not_allowed(name) ->
        Format.eprintf " call to \\%s not allowed in document preamble\n" (txt name)
  end;
  Format.pp_print_newline Format.err_formatter ()

(** Main entry point to the Earley grammar. This functions catches any
    error raised by the parser and prints them to standard error. *)
let parse_buffer buffer =
  try
    Earley.parse_buffer document latex_blank buffer
  with
  | EarleyEngine.Parse_error(buf, pos) ->
      let loc = Pa_ocaml_prelude.locate buf pos buf pos in
      print_syntax_error loc Syntax_error; exit 1
  | Latex_syntax_error(pos, err) -> print_syntax_error pos err; exit 1

(** Pretty-printing of the result tree, obtained after parsing, for
    debugging purpose. *)
let debug_printer doc =
  let open LatexAst_iterator in
  let print_document_class iterator dc =
    Printf.fprintf stderr "\\documentclass{%s}\n" (txt dc.cls_name)
  in
  let print_environment iterator env =
    Printf.fprintf stderr "\\begin{%s}\n" (txt env.env_name);
    default_iterator.environment iterator env;
    Printf.fprintf stderr "\\end{%s}\n" (txt env.env_name)
  in
  let print_macro iterator macro =
    Printf.fprintf stderr "\\%s" (txt macro.mac_name)
  in
  let print_text _ text =
    Printf.fprintf stderr "Text(%s)" (txt text)
  in
  let print_paragraph iterator par =
    default_iterator.paragraph iterator par;
    Printf.fprintf stderr "\n\n"
  in
  let print_iterator = { default_iterator with
    documentclass = print_document_class;
    environment = print_environment;
    text = print_text;
    macro = print_macro;
    paragraph = print_paragraph;
  }
  in print_iterator.document print_iterator doc

(** Main function *)
let main () =
  ArgSpec.parse ();

  let buffer = match !ArgSpec.file with
  | None -> Input.from_channel ~filename:"__stdin__" stdin
  | Some(fname) ->
      try Input.from_file fname
      with Sys_error(_) -> Input.from_file (fname ^ ".tex")
  in

  let res = parse_buffer buffer in
  debug_printer res;

  Printf.fprintf stdout "No error found! Perhaps a missing \\item?\n%!"

let _ =
  main ()
