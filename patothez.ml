open Parser

let print_option = function
  Bool key -> Printf.printf "Option '%s' set\n" key
  | Val(key, value) -> Printf.printf "Option '%s' = '%s'\n" key value

let _ =
  (try
    let res = Parser.main (Dyp.from_channel (Parser.pp ()) stdin) in
    Printf.fprintf stderr "%d parse trees\n" (List.length res);
    let ((docclass, doccont), _)::_ = res in
      Printf.fprintf stderr "Documentclass: %s\n" docclass.name;
      List.iter print_option docclass.options;
      (*let out = open_out "thez.tml" in *)
      Generateur.gen_ml "thez" doccont (*out*);
  with
    Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"; exit 1
  | Parser.Syntax_Error(pos, err) ->
      LatexSyntax.print_syntax_error pos.Lexing.pos_fname pos err; exit 1
  );
  flush stdout;
  Printf.fprintf stderr "No error found! Perhaps a missing \\item?\n"
