type syntax_error =
  | Unmatched_environment of string * string
  | Begin_document of string

let print_syntax_error file pos msg =
  match msg with
    Unmatched_environment(a, b) ->
      Printf.fprintf stderr "Environment \"%s\" ended by \"%s\"\n" a b 
  | Begin_document(a) ->
      Printf.fprintf stderr "\\begin{document} is expected, instead of \"%s\"\n" a

