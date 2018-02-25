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
  end;
  Format.pp_print_newline Format.err_formatter ()

let _ =
  (try
    let res = Earley.parse_channel ~filename:"__stdin__"
    document latex_blank stdin in
    Printf.fprintf stderr "Documentclass: %s\n" res.doc_cls.cls_name.Location.txt;
    List.iter print_option res.doc_cls.cls_options;
  with
    EarleyEngine.Parse_error(buf, pos) -> Printf.printf "Syntax error\n\n"; exit 1
  | Latex_syntax_error(pos, err) -> print_syntax_error pos err; exit 1
  );
  flush stdout; flush stderr;
  Printf.fprintf stderr "No error found! Perhaps a missing \\item?\n"
