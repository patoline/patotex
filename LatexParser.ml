(* SPDX-License-Identifier: GPL-3.0 *)
(** Parser for simple LaTeX files *)

open Earley
open LatexAst_helper

let locate = Pa_ocaml_prelude.locate
#define LOCATE locate

type latex_syntax_error =
  | Syntax_error
  | Unmatched_environment of string Location.loc * string Location.loc
  | Unterminated_environment of string Location.loc
  | Document_environment
  | Preamble_not_allowed of string Location.loc

exception Latex_syntax_error of loc * latex_syntax_error

let raise_latex ?(loc=!default_loc) err =
  raise (Latex_syntax_error(loc, err))

(** Usual blank functions for LaTeX, which ignores comments starting
with a [%] sign an ending at the end of line. *)
let latex_blank =
  blank_regexp ''\(\(%[^\n]*\n\)\|[ \t\n]\)*''

let latex_paragraph_blank =
  let rec fn accept_newline buffer pos =
    let (c, buffer', pos') = Input.read buffer pos in
    match c with
    | '\n' when accept_newline -> fn false buffer' pos'
    | ' ' | '\t' | '\r' -> fn accept_newline buffer' pos'
    | _ -> buffer, pos
  in fn true

(** Blank function for verbatim content, which does not ignore any
character *)
let verbatim_blank buf pos = (buf, pos)

let identifier = parser i:''[a-zA-Z_][a-zA-Z0-9_]*'' -> mkloc i _loc

let macro_name = parser
  '\\' mac_name:identifier -> if txt mac_name = "begin" then give_up ()
  else mac_name

let documentclass_option = parser
  | key:identifier -> Latex.ClsFlag key
  | key:identifier '=' value:{v:''[^]]+''-> mkloc v _loc} -> Latex.ClsVal(key, value)

let parser documentclass_options =
  opts:{
    '[' o1:documentclass_option
    o2:{ ',' o:documentclass_option }* ']' -> o1 :: o2
  }? ->
    match opts with
      | Some(l) -> l
      | None -> []

let documentclass = parser
  "\\documentclass"
  options:documentclass_options
  '{' name:identifier '}' ->
    DocCls.mk ~_loc ~options name

let regular_text = parser
  t:''[^{}%\\\n]+'' -> mkloc t _loc

let environment_end env_name = parser
  "\\end{" env_close_name:identifier '}' ->
    if txt env_close_name <> txt env_name
    then raise_latex ~loc:_loc (Unmatched_environment(env_name, env_close_name))
  | EOF -> raise_latex ~loc:_loc (Unterminated_environment(env_name))

let parser environment =
  "\\begin{" env_name:identifier '}' ->>
  c:content (environment_end env_name) ->
    Latex.({env_name; env_content = c; env_loc = _loc})

and parser paragraph_item =
    t:regular_text -> Latex.Text(t)
  | m:macro_name -> Latex.(Macro({
    mac_name = m; mac_optarg = None; mac_args = [];
    mac_is_starred=false; mac_loc = _loc})) (* TODO better macro
    handling *)

and parser paragraph = (change_layout (parser
  p:paragraph_item+ '\n'? -> Latex.({par_content = p; par_loc = _loc})
) latex_paragraph_blank)

and parser content_item =
    e:environment -> Latex.Environment(e)
  | p:paragraph -> Latex.Paragraph(p)

and parser content =
  content_item*

let package_declaration = parser
  mac_name:macro_name
  pkg_params:documentclass_options
  '{' pkg_name:identifier '}' ->
    if txt mac_name = "usepackage"
    then Latex.({ pkg_name; pkg_params; pkg_loc = _loc })
    else if txt mac_name = "begin"
    then give_up()
    else raise_latex ~loc:_loc (Preamble_not_allowed(mac_name))

let preamble = parser
  p:package_declaration* -> Latex.({ pre_packages = p; pre_loc = _loc})

let document = parser
  doc_cls:documentclass
  doc_preamble:preamble
  doc:environment ->
    if txt doc.Latex.env_name <> "document"
    then raise_latex ~loc:doc.Latex.env_loc Document_environment
    else Latex.({doc_cls; doc_preamble; doc_content =
    doc.env_content})
