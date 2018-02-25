open Latex
open LatexParser
open LatexAst_helper

let print_option = function
  | ClsFlag(key) -> Printf.printf "Option '%s' set\n" key.Location.txt
  | ClsVal(key, value) -> Printf.printf "Option '%s' = '%s'\n"
  key.Location.txt value.Location.txt

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

let parse_buffer buffer =
  try
    Earley.parse_buffer document latex_blank buffer
  with
  | EarleyEngine.Parse_error(buf, pos) ->
      let loc = Pa_ocaml_prelude.locate buf pos buf pos in
      print_syntax_error loc Syntax_error; exit 1
  | Latex_syntax_error(pos, err) -> print_syntax_error pos err; exit 1

let main () =
  let buffer = Input.from_channel ~filename:"__stdin__" stdin in
  let res = parse_buffer buffer in
  Printf.fprintf stderr "Documentclass: %s\n" res.doc_cls.cls_name.Location.txt;
  List.iter print_option res.doc_cls.cls_options;
  flush stdout; flush stderr;
  Printf.fprintf stderr "No error found! Perhaps a missing \\item?\n"

let _ =
  main ()
