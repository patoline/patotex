open Latex
open LatexParser

let print_option = function
  | ClsFlag(key) -> Printf.printf "Option '%s' set\n" key.Location.txt
  | ClsVal(key, value) -> Printf.printf "Option '%s' = '%s'\n"
  key.Location.txt value.Location.txt

let print_syntax_error pos err =
  Location.(print_error Format.err_formatter pos);
  match err with
  | Unmatched_environment(start, close) ->
      Format.fprintf Format.err_formatter " unmatched environment\n\\begin{%s} was closed with \\end{%s}\n"
      (LatexAst_helper.txt start)
      (LatexAst_helper.txt close);
      Location.(print_loc Format.err_formatter start.loc);
      Format.fprintf Format.err_formatter ": Start tag was here\n"
  | Document_environment ->
      Format.fprintf Format.err_formatter " document content must be enclosed inside \\begin{document} and \\end{document}\n"

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
  flush stdout;
  Printf.fprintf stderr "No error found! Perhaps a missing \\item?\n"
