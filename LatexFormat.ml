open Typography
open Typography.Document

module type LatexStructure = sig
  include Typography.Document.DocumentStructure
  val at_most : int ref

  val doc_title : string ref
  val doc_tags : ((string * string) list) ref
end

type sectionning = Chapter | Section | Subsection

let go_at_level structure level =
  let level = match level with
  | Chapter -> 1
  | Section -> 2
  | Subsection -> 3
  in let (tree, context) = !structure in
  let depth = List.length context in
  for i = level to depth do
    go_up structure
  done

(** Macros calls at runtime, when producing the PDF output *)
module Format (DocStruct : LatexStructure) = struct
  module DefaultF = FormatArticle.Format(DocStruct)
  include DefaultF

  let maketitle () =
    Default.title DocStruct.structure ~extra_tags:!DocStruct.doc_tags
    [tT(!DocStruct.doc_title)]

  let title cont =
    DocStruct.doc_title := cont

  let author cont =
    DocStruct.doc_tags := ("Author", cont) :: !DocStruct.doc_tags

  let sectionning level ?(is_starred = false) cont =
    go_at_level DocStruct.structure level;
    newStruct ~numbered:(not is_starred) DocStruct.structure [tT cont]

  let chapter = sectionning Chapter
  let section = sectionning Section
  let subsection = sectionning Subsection

  (* Inline macros *)
  let macro () = tT "macro"

  (* Math macros *)
  module Math = struct
      let make_a_glyph g () = failwith "not imp"
        (* (Maths.Ordinary (Maths.noad (Maths.glyphs g)) : Document.user Maths.math) *)
      let ccat = make_a_glyph "C"
      let bot  = make_a_glyph "⊥"
      let neg  = make_a_glyph "¬"
  end
end

(** Helper functions for parsing, which tells the parser which macro calls are
 * expected to be inline (i.e., inside a paragraph and which produre some
 * textual content), or block (i.e., whose return value does not interest us).
 *)
module Grammar = struct
  let block_content =
    ["title"; "maketitle"; "author"; "chapter"; "section"; "subsection";
    "subsubsection"]
end
