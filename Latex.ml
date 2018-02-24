(** Abstract Syntax Tree for simple LaTeX files

We define in this module an Abstract Syntax Tree representation for
LaTeX files which stick to the basic LaTeX syntax (i.e., which do not
play with catcodes and do not rely on macro expansion for parsing).
*)

(** Representing a located value, i.e., a value of type ['a] along with
its location in the source file. *)
type 'a loc = 'a Location.loc

(** Document class option.

When a document starts with
[\documentclass[option1,option2=value]{class}], this type represents
[option1] using the [ClsFlag] constructor and [option2=value] using the
[ClsVal] constructor. *)
type documentclass_option =
  | ClsFlag of string loc
  | ClsVal of string loc * string loc

(** Representing the initial [\documentclass] call *)
type documentclass = {
  cls_name : string loc;
  cls_options : documentclass_option list;
  cls_loc : Location.t;
}

(** Node type for macro calls, parameterized by the type of its
arguments. In math context, we use [math_content macro_call], while in
non-math context, we use [content macro_call].

We expect that macro names can be mapped to OCaml function names. Yet,
this is not the case for names with a trailing "*". Most of them (if not
all) are only a slight variation on the non-starred macro. We thus
handle them using the [mac_is_starred] boolean.
*)
type 'a macro_call = {
  mac_name : string loc;
  mac_optarg : 'a option;
  mac_args : 'a list;
  mac_is_starred : bool;
  mac_loc : Location.t;
}

(** Environments [\begin{env_name} content \end{env_name}]. *)
and environment = {
  env_name : string loc;
  env_content : content list;
  env_loc : Location.t;
}

(** LaTeX paragraphs.

The given type puts no restriction on content which can appear inside a
paragraph, yet the LaTeX syntax cannot for example build an environment
nested in a paragraph. *)
and paragraph = {
  par_content : content list;
  par_loc : Location.t;
}

(** Main content inside a document *)
and content =
  | Environment of environment
  | Macro of content macro_call
  | Paragraph of paragraph
  | Math of math
  | Tikz of tikz

(** Tikz drawings. *)
and tikz = {
  tikz_content : tikz_content list;
  tikz_loc : Location.t;
}

and tikz_coord =
  | Cart of float loc * float loc
  | Named of string loc
  | Polar of float loc * float loc (* angle, distance *)
  | Barycentric of (string loc * float loc) list

and tikz_move =
  | Abs of tikz_coord
  | Rel of tikz_coord
  | Rel_move of tikz_coord

and tikz_node =
  { node_name : string; node_pos : tikz_coord; node_text : content list }

and tikz_path =
  | Move_to of tikz_move
  | Line_to of tikz_move
  | Node of tikz_node

and tikz_content =
  | Path of tikz_path list

(** Mathematical formulas. *)
and math = {
  math_content : math_content list;
  math_loc : Location.t;
}

and math_symbol =
    SimpleSym of string
  | CamlSym of string

and math_content =
     Var of string
   | Symbol of math_symbol
   | Fun of string
   | Num of string
   | Prefix of int * math_symbol * bool * math_content
   | Operator of string * math_content
   | Limits_operator of string * math_content
   | Postfix of int * math_content * bool * math_symbol
   | Binary of int * math_content * bool * math_symbol * bool * math_content
   | Indices of indices * math_content
   | Apply of math_content * math_content
   | MathMacro of math_content macro_call
   | Delim of string * math_content * string
   | MScope of math_content list
   | MathString of string

(** Sub- and superscripts for math contents *)
and indices = {
  up_right : math_content option;
  down_right : math_content option;
  up_left : math_content option;
  down_left : math_content option
}

type package_declaration = {
  pkg_name : string loc;
  pkg_params : documentclass_option list;
  pkg_loc : Location.t;
}
(** Preamble *)
type preamble = {
  pre_packages: package_declaration list;
  pre_loc : Location.t;
}

(** Main type of the syntax tree, returned by the parser *)
type document = {
  doc_cls : documentclass;
  doc_preamble : preamble;
  doc_content : content list;
}
