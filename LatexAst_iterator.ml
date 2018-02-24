(** Inspecting the LaTeX AST with open recursion.

The {!val:default_iterator} traverses a syntax tree without actually
doing anything. One would typically build his own iterator using
{!val:default_iterator} and override handlers for some syntax items.
*)

open Latex

(** An {!type:iterator} record consists of one method for each
category of syntax tree items. This first argument of each method is the
iterator which should be applied to child items of the considered tree
element. *)
type iterator = {
  document : iterator -> document -> unit;
  documentclass : iterator -> documentclass -> unit;
  preamble : iterator -> preamble -> unit;
  content : iterator -> content -> unit;
  environment : iterator -> environment -> unit;
  paragraph : iterator -> paragraph -> unit;
  macro : iterator -> content macro_call -> unit;
  math : iterator -> math -> unit;
  math_content : iterator -> math_content -> unit;
  math_macro : iterator -> math_content macro_call -> unit;
}

let generic_macro_iterator iterator macro =
  (match macro.mac_optarg with
    | Some(c) -> iterator c
    | None -> ());
    List.iter iterator macro.mac_args

(** A default iterator which traverses the whole tree, applying each
{!type:iterator} method to all possible tree items, but without doing
anything during the traversal. *)
let default_iterator = {
  document = (fun this document ->
    this.documentclass this document.doc_cls;
    this.preamble this document.doc_preamble;
    List.iter (this.content this) document.doc_content
  );

  documentclass = (fun _ _ -> ());

  preamble = (fun _ _ -> ());

  content = (fun this content ->
    match content with
    | Environment(env) -> this.environment this env
    | Paragraph(par) -> this.paragraph this par
    | Math(math) -> this.math this math
    | Macro(macro) -> this.macro this macro
    | Tikz(tikz) -> failwith "not implemented"   (** FIXME *)
  );

  environment = (fun this env ->
    List.iter (this.content this) env.env_content);

  paragraph = (fun this par ->
    List.iter (this.content this) par.par_content);

  macro = (fun this macro ->
    generic_macro_iterator (this.content this) macro);

  math = (fun this math ->
    List.iter (this.math_content this) math.math_content);

  math_content = (fun this math_content ->
    match math_content with
    | Prefix(_, _, _, c)
    | Operator(_, c)
    | Limits_operator(_, c)
    | Postfix(_, c, _, _)
    | Indices(_, c)
    | Delim(_, c, _) ->
        this.math_content this c

    | MScope(cl) -> List.iter (this.math_content this) cl

    | Binary(_, c1, _, _, _, c2)
    | Apply(c1, c2) ->
        this.math_content this c1;
        this.math_content this c2;

    | MathMacro(call) -> this.math_macro this call

    | _ -> ()
  );

  math_macro = (fun this macro ->
    generic_macro_iterator (this.math_content this) macro);
}
