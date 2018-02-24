(** Parser for simple LaTeX files *)

open Earley
open LatexAst_helper

let locate = Pa_ocaml_prelude.locate
#define LOCATE locate

(** Usual blank functions for LaTeX, which ignores comments starting
with a [%] sign an ending at the end of line. *)
let blank =
  blank_regexp ''%.*$''

(** Blank function for verbatim content, which does not ignore any
character *)
let verbatim_blank buf pos = (buf, pos)

let identifier = parser i:''[a-zA-Z_][a-zA-Z0-9_]*'' -> mkloc i _loc

let macro_name = parser
  '\\' mac_name:identifier -> mac_name

(** TODO implement *)
let documentclass_options = parser
  EMPTY -> []

let documentclass = parser
  "\\documentclass"
  cls_options:documentclass_options
  '{' cls_name:identifier '}' ->
    Latex.({cls_name; cls_options; cls_loc = _loc})

let environment = parser
  "\\begin{" env_name:identifier '}' ->>
  (** TODO add content *)
  "\\end{" STR(txt env_name) '}' ->
    Latex.({env_name; env_content = []; env_loc = _loc})

(** TODO implement *)
let preamble = parser
  EMPTY -> Latex.({ pre_packages = []; pre_loc = _loc})

let document = parser
  doc_cls:documentclass
  doc_preamble:preamble
  doc:environment -> Latex.({doc_cls; doc_preamble; doc_content =
    doc.env_content})
