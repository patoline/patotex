open Camlp4.PreCast

module TikzGenerateur = struct
  open Parser

  type tikz_pos = float * float
  type 'a state = tikz_pos -> 'a * tikz_pos

  let bind : 'a state -> ('a -> 'b state) -> 'b state =
    fun st f ps -> let (x, y) = st ps in f x y
  let (>>) a b = bind a b
  let return : 'a -> 'a state =
    fun a ps -> (a, ps)

  let to_cart : tikz_coord -> tikz_pos = fun pos ->
    match pos with
    | Cart(x, y) -> (x, y)

  let add : tikz_pos -> tikz_coord -> tikz_pos =
    fun (x2, y2) dest ->
      let (x1, y1) = to_cart dest in (x1 +. x2, y1 +. y2)

  let move : tikz_move -> tikz_pos state =
    fun move pos ->
      match move with
      | Rel(dest) -> (add pos dest, pos)
      | Rel_move(dest) -> let newpos = add pos dest in (newpos, newpos)
      | Abs(dest) -> let newpos = to_cart dest in (newpos, newpos)

  let walk (p : tikz_pos state) = snd (p (0., 0.))

end

(* Les macros sont compilées vers Format.nom_macro(arguments) *)
let rec string_of_contents = function
  | Parser.Environment(s, cl) -> String.concat "" (List.map string_of_contents cl)
  | Parser.Macro _ -> ""
  | Parser.Paragraph cl -> String.concat "" (List.map string_of_contents cl)
  | Parser.Text t -> t
  | Parser.Math _ -> ""
  | Parser.Tikz _ -> ""

let plusplus r =
  let x = !r in
  incr r; x

let trim s =
  let l = String.length s in
  let pre_space = ref 0 in
  while !pre_space < l && s.[!pre_space] = ' ' do
    incr pre_space
  done;
  let post_space = ref (l - 1) in
  while !post_space > !pre_space && s.[!post_space] = ' ' do
    decr post_space
  done;
  let res_length = !post_space - !pre_space + 1 in
  let res = String.make res_length ' ' in
  String.blit s (!pre_space) res 0 res_length; res

let rec merge_contiguous_text = function
  [] -> []
  | [_] as sa -> sa
  | Parser.Text(a)::Parser.Text(b)::tl -> merge_contiguous_text (Parser.Text(a ^ b) :: tl)
  | Parser.Text(a)::tl -> (* Text followed by non text *)
      (let ta = trim a in if ta = "" then [] else [Parser.Text(ta)]) @
      (if a.[String.length a - 1] = ' ' then [Parser.Text " "] else []) @
      (merge_contiguous_text tl)
  | a :: (Parser.Text(b) :: tl as cl) -> (* Non text followed by text *)
      a :: (
        (if b.[0] = ' ' then [Parser.Text " "] else [])
        @
        (merge_contiguous_text cl)
      )
  | a::tl -> a :: (merge_contiguous_text tl)

let normalize_text cl =
  match merge_contiguous_text cl with
  | Parser.Text(a) :: tl -> Parser.Text(trim a) :: tl
  | cl -> cl

let make_macro_call ?(modul="Format") mac =
  let cl = mac.Parser.args in
  let _loc = __LOCATION__ in
  <:expr<
  $uid:modul$.$lid:mac.Parser.mac_name$
  >>
  (*Printf.sprintf "(%s.%s %s %s %s)"
    modul
    mac.Parser.mac_name
    (if mac.Parser.is_starred then "~is_starred:true" else "")
    (match mac.Parser.optarg with
      None -> ""
     | Some cl -> "~opt:\"yopla\""
    )
    (if cl = []
    then "()"
    else "\"" ^ (String.concat "" (List.map string_of_contents cl)) ^
    "\"")*)

let make_math mc =
  let _loc = __LOCATION__ in <:expr< () >>
  (*let make_one_math = function
    Parser.MathMacro mac -> Printf.sprintf "Format.Math.%s()" mac.Parser.mac_name
    | Parser.Var mac -> ""
  in String.concat ";" (List.map make_one_math mc)*)

let rec make =
  let temp_count = ref 0 in
  fun doc ->
  match doc with
  | Parser.Environment(s, cl) ->
      let inner = Ast.stSem_of_list (List.map make cl) in
      let _loc = __LOCATION__ in
      <:str_item<
      module $"TEMP" ^ (string_of_int !temp_count)$ = struct
        open $uid:"Env_" ^ s$

        let _ = do_begin_env ();;

        $inner$

        let _ = do_end_env();;
      end
      >>
  | Parser.Macro mac -> let _loc = __LOCATION__ in <:str_item<let _ = $make_macro_call mac$>>
  | Parser.Paragraph cl ->
      let par_contents =
        (List.map (function
          Parser.Text t -> let _loc = __LOCATION__ in <:expr<tT $str:t$>>
          | Parser.Macro mac -> make_macro_call mac
          | Parser.Math mc -> let _loc = __LOCATION__ in <:expr<bB (fun env -> Maths.draw [env] [$make_math mc$])>>
          | _ -> let _loc = __LOCATION__ in <:expr< () >>
        ) (normalize_text cl))
      in
      let _loc = __LOCATION__ in
      <:str_item<
        let _ = newPar DocStruct.structure Complete.normal parameters
        [$Ast.exSem_of_list par_contents$]
      >>
  | Parser.Text t -> let _loc = __LOCATION__ in <:str_item< >>
  | Parser.Math _ -> let _loc = __LOCATION__ in <:str_item< >>
  | Parser.Tikz _ -> let _loc = __LOCATION__ in <:str_item< >>

let preamble =
  let _loc = __LOCATION__ in
  <:str_item<
open Typography
open Typography.Util
open Typography.Box
open Typography.Config
open Typography.Document
open Typography.OutputCommon

module DocStruct = (
  struct
    let structure = ref (Node { empty with node_tags=["InTOC",""] },[])
    let fixable = ref false
    let doc_title = ref ""
    let doc_tags = ref []
    let at_most = ref 3
  end : Thezformat.ThezStructure)

module Format = Thezformat.Format(DocStruct)

open Format
open DefaultFormat.MathsFormat
>>

let postamble fileprefix =
  let filename = fileprefix ^ ".pdf" in
  let _loc = __LOCATION__ in
  <:str_item<
module Out = FormatArticle.Output(Pdf)

let _ =
  let filename = $str:filename$ in
  let rec resolve i env0=
  DocStruct.fixable:=false;
  let tree=postprocess_tree (fst (top (!DocStruct.structure))) in
  let env1,fig_params,params,compl,pars,figures=flatten env0 DocStruct.fixable tree in
  let (logs,pages,figs',user')=TS.typeset
    ~completeLine:compl
    ~figure_parameters:fig_params
    ~figures:figures
    ~parameters:params
    ~badness:(Badness.badness pars figures)
    pars
  in
  let env2, reboot=update_names env1 figs' user' in
  if i < !DocStruct.at_most-1 && reboot && !DocStruct.fixable then (
    resolve (i+1) env2
  ) else (
    Out.output tree pars figures env2 pages filename
  )
  in
  let env0=defaultEnv in
  resolve 0 env0
>>

let gen_ml filename doc =
  Printers.OCaml.print_implem preamble;
  List.iter (fun d -> Printers.OCaml.print_implem (make d));
  Printers.OCaml.print_implem (postamble filename)
