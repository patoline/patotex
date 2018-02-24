(** Parser for simple LaTeX files *)

open Earley
open LatexAst_helper

let locate = Pa_ocaml_prelude.locate
#define LOCATE locate

type latex_syntax_error =
  | Unmatched_environment of string Location.loc * string Location.loc
  | Document_environment

exception Latex_syntax_error of loc * latex_syntax_error

let raise_latex ?(loc=!default_loc) err =
  raise (Latex_syntax_error(loc, err))

(** Usual blank functions for LaTeX, which ignores comments starting
with a [%] sign an ending at the end of line. *)
let latex_blank =
  blank_regexp ''\(\(%[^\n]*\n\)\|[ \t\n]\)*''

(** Blank function for verbatim content, which does not ignore any
character *)
let verbatim_blank buf pos = (buf, pos)

let identifier = parser i:''[a-zA-Z_][a-zA-Z0-9_]*'' -> mkloc i _loc

let macro_name = parser
  '\\' mac_name:identifier -> mac_name

let documentclass_option = parser
  | key:identifier -> Latex.ClsFlag key
  | key:identifier '=' value:{v:''[^]]+''-> mkloc v _loc} -> Latex.ClsVal(key, value)

let documentclass_options = parser
  '[' o1:documentclass_option?
  o2:{ ',' o:documentclass_option }* ']' ->
    match o1 with
    | Some(o1) -> o1 :: o2
    | None -> []

let documentclass = parser
  "\\documentclass"
  cls_options:documentclass_options
  '{' cls_name:identifier '}' ->
    Latex.({cls_name; cls_options; cls_loc = _loc})

let environment_end env_name = parser
  "\\end{" env_close_name:identifier '}' ->
    if txt env_close_name = txt env_name
    then Latex.({env_name; env_content = []; env_loc = _loc})
    else raise_latex ~loc:_loc (Unmatched_environment(env_name, env_close_name))

let parser environment =
  "\\begin{" env_name:identifier '}' ->>
  content
  e:(environment_end env_name) -> { e with Latex.env_loc = _loc }

and parser content =
  environment*

(** TODO implement *)
let preamble = parser
  EMPTY -> Latex.({ pre_packages = []; pre_loc = _loc})

let document = parser
  doc_cls:documentclass
  doc_preamble:preamble
  doc:environment ->
    if txt doc.Latex.env_name <> "document"
    then raise_latex ~loc:doc.Latex.env_loc Document_environment
    else Latex.({doc_cls; doc_preamble; doc_content =
    doc.env_content})
