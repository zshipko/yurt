module Tag = struct
  type t =
    [ `A
    | `ADDR
    | `ADDRESS
    | `AREA
    | `ARTICLE
    | `ASIDE
    | `AUDIO
    | `B
    | `BASE
    | `BLOCKQUOTE
    | `BODY
    | `BR
    | `BUTTON
    | `CANVAS
    | `CAPTION
    | `CODE
    | `COL
    | `COLGROUP
    | `DATALIST
    | `DD
    | `DETAILS
    | `DIV
    | `DL
    | `DT
    | `EM
    | `EMBED
    | `FIELDSET
    | `FIGCAPTION
    | `FIGURE
    | `FOOTER
    | `FORM
    | `H1
    | `H2
    | `H3
    | `H4
    | `H5
    | `H6
    | `HEAD
    | `HEADER
    | `HGROUP
    | `HR
    | `HTML
    | `I
    | `IFRAME
    | `IMG
    | `INPUT
    | `KEYGEN
    | `LABEL
    | `LEGEND
    | `LI
    | `LINK
    | `META
    | `MAIN
    | `NAV
    | `NOSCRIPT
    | `OBJECT
    | `OL
    | `OPTGROUP
    | `OPTION
    | `P
    | `PRE
    | `PROGRESS
    | `Q
    | `SCRIPT
    | `SECTION
    | `SELECT
    | `SOURCE
    | `SPAN
    | `STRONG
    | `STYLE
    | `SUB
    | `SUP
    | `TABLE
    | `TBODY
    | `TD
    | `TEXTAREA
    | `TFOOT
    | `TH
    | `THEAD
    | `TIME
    | `TITLE
    | `TR
    | `TRACK
    | `U
    | `UL
    | `VIDEO
    | `WBR
    | `RAW
    | `NAME of string
    | `VAR of string ]

  let to_string (tag : t) : string =
    match tag with
    | `A -> "a"
    | `ADDR -> "addr"
    | `ADDRESS -> "address"
    | `AREA -> "area"
    | `ARTICLE -> "article"
    | `ASIDE -> "aside"
    | `AUDIO -> "audio"
    | `B -> "b"
    | `BASE -> "base"
    | `BLOCKQUOTE -> "blockquote"
    | `BODY -> "body"
    | `BR -> "br"
    | `BUTTON -> "button"
    | `CANVAS -> "canvas"
    | `CAPTION -> "caption"
    | `CODE -> "code"
    | `COL -> "col"
    | `COLGROUP -> "colgroup"
    | `DATALIST -> "datalist"
    | `DD -> "dd"
    | `DETAILS -> "details"
    | `DIV -> "div"
    | `DL -> "dl"
    | `DT -> "dt"
    | `EM -> "em"
    | `EMBED -> "embed"
    | `FIELDSET -> "fieldset"
    | `FIGCAPTION -> "figcaption"
    | `FIGURE -> "figure"
    | `FOOTER -> "footer"
    | `FORM -> "form"
    | `H1 -> "h1"
    | `H2 -> "h2"
    | `H3 -> "h3"
    | `H4 -> "h4"
    | `H5 -> "h5"
    | `H6 -> "h6"
    | `HEAD -> "head"
    | `HEADER -> "header"
    | `HGROUP -> "hgroup"
    | `HR -> "hr"
    | `HTML -> "html"
    | `I -> "i"
    | `IFRAME -> "iframe"
    | `IMG -> "img"
    | `INPUT -> "input"
    | `KEYGEN -> "keygen"
    | `LABEL -> "label"
    | `LEGEND -> "legend"
    | `LI -> "li"
    | `LINK -> "link"
    | `MAIN -> "main"
    | `META -> "meta"
    | `NAV -> "nav"
    | `NOSCRIPT -> "noscript"
    | `OBJECT -> "object"
    | `OL -> "ol"
    | `OPTGROUP -> "optgroup"
    | `OPTION -> "option"
    | `P -> "p"
    | `PRE -> "pre"
    | `PROGRESS -> "progress"
    | `Q -> "q"
    | `SCRIPT -> "script"
    | `SECTION -> "section"
    | `SELECT -> "select"
    | `SOURCE -> "source"
    | `SPAN -> "span"
    | `STRONG -> "strong"
    | `STYLE -> "style"
    | `SUB -> "sub"
    | `SUP -> "sup"
    | `TABLE -> "table"
    | `TBODY -> "tbody"
    | `TD -> "td"
    | `TEXTAREA -> "textarea"
    | `TFOOT -> "tfoot"
    | `TH -> "th"
    | `THEAD -> "thead"
    | `TIME -> "time"
    | `TITLE -> "title"
    | `TR -> "tr"
    | `TRACK -> "track"
    | `U -> "u"
    | `UL -> "ul"
    | `VIDEO -> "video"
    | `WBR -> "wbr"
    | `RAW -> "raw"
    | `NAME s -> s
    | `VAR _ -> failwith "Cannot convert var to string"

  let nonclosing = [ `IMG; `INPUT; `HR; `META; `BR; `WBR; `LINK ]
end

type t = {
  tag : Tag.t;
  mutable attrs : (string * string) list;
  mutable content : string option;
  mutable children : t list;
}
(** HTML node type *)

let rec replace (a : t) (name : string) (b : t) : t =
  match a.tag with
  | `VAR s when s = name -> b
  | _ ->
      {
        tag = a.tag;
        attrs = a.attrs;
        content = a.content;
        children = List.map (fun i -> replace i name b) a.children;
      }

let tag ?(attr = []) ?(content = None) ?(children = []) (tag : Tag.t) : t =
  { tag; attrs = attr; content; children }

let var name = tag (`VAR name)

let import name =
  let ic = open_in name in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  tag ~content:(Some (Bytes.to_string s)) `RAW

let html ?(attr = []) ?content children = tag ~attr ~content ~children `HTML

let body ?(attr = []) ?content children = tag ~attr ~content ~children `BODY

let head ?(attr = []) ?content children = tag ~attr ~content ~children `HEAD

let meta attr = tag ~attr `META

let script ?(attr = []) content = tag ~attr ~content:(Some content) `SCRIPT

let canvas ?content attr = tag ~attr ~content `CANVAS

let style ?(attr = []) content = tag ~attr ~content:(Some content) `STYLE

let iframe attr = tag ~attr `IFRAME

let div ?(attr = []) ?content children = tag ~attr ~content ~children `DIV

let p ?(attr = []) ?(children = []) content =
  tag ~attr ~content:(Some content) ~children `P

let span ?(attr = []) ?content children = tag ~attr ~content ~children `SPAN

let h1 ?(attr = []) ?(children = []) content =
  tag ~attr ~content:(Some content) ~children `H1

let h2 ?(attr = []) ?(children = []) content =
  tag ~attr ~content:(Some content) ~children `H2

let h3 ?(attr = []) ?(children = []) content =
  tag ~attr ~content:(Some content) ~children `H3

let h4 ?(attr = []) ?(children = []) content =
  tag ~attr ~content:(Some content) ~children `H4

let h5 ?(attr = []) ?(children = []) content =
  tag ~attr ~content:(Some content) ~children `H5

let h6 ?(attr = []) ?(children = []) content =
  tag ~attr ~content:(Some content) ~children `H6

let header ?(attr = []) ?content children = tag ~attr ~content ~children `HEADER

let main ?(attr = []) ?content children = tag ~attr ~content ~children `MAIN

let nav ?(attr = []) ?content children = tag ~attr ~content ~children `NAV

let article ?(attr = []) ?content children =
  tag ~attr ~content ~children `ARTICLE

let section ?(attr = []) ?content children =
  tag ~attr ~content ~children `SECTION

let aside ?(attr = []) ?content children = tag ~attr ~content ~children `ASIDE

let footer ?(attr = []) ?content children = tag ~attr ~content ~children `FOOTER

let ul ?(attr = []) ?content children = tag ~attr ~content ~children `UL

let ol ?(attr = []) ?content children = tag ~attr ~content ~children `OL

let li ?(attr = []) ?content children = tag ~attr ~content ~children `LI

let text ?(attr = []) (s : string) = tag ~attr ~content:(Some s) `SPAN

let label ?(attr = []) (s : string) = tag ~attr ~content:(Some s) `LABEL

let link ?(children = []) ?content attr = tag ~attr ~content ~children `LINK

let a ?(children = []) attr content =
  tag ~attr ~content:(Some content) ~children `A

let img attr = tag ~attr `IMG

let textarea ?(attr = []) (s : string) = tag ~attr ~content:(Some s) `TEXTAREA

let input attr = tag ~attr `INPUT

let select ?(attr = []) children = tag ~attr ~children `SELECT

let opt ?(attr = []) (s : string) = tag ~attr ~content:(Some s) `OPTION

let form ?(attr = []) children = tag ~attr ~children `FORM

let mk_form ?attr attrs submit_text =
  form ?attr
    ( List.map (fun a -> input a) attrs
    @ [ input [ ("type", "submit"); ("value", submit_text) ] ] )

let audio ?(attr = []) ?content children = tag ~attr ~content ~children `AUDIO

let video ?(attr = []) ?content children = tag ~attr ~content ~children `VIDEO

let source attr = tag ~attr `SOURCE

let title content = tag ~content:(Some content) `TITLE

let inline (s : string) = tag ~content:(Some s) `RAW

let rec string_of_attrs (node : t) : string =
  (if List.length node.attrs > 0 then " " else "")
  ^ String.concat " " (List.map (fun (k, v) -> k ^ "=\"" ^ v ^ "\"") node.attrs)

and string_of_content (node : t) : string =
  match node.content with Some s -> s | None -> ""

and string_of_children (node : t) : string =
  String.concat "\n" (List.map to_string node.children)

and to_string (node : t) : string =
  let tag = Tag.to_string node.tag in
  match node with
  | _ when tag = "raw" -> ( match node.content with Some s -> s | None -> "" )
  | _ when List.mem node.tag Tag.nonclosing ->
      "<" ^ tag ^ string_of_attrs node ^ " />"
  | _ ->
      "<" ^ tag ^ string_of_attrs node ^ ">" ^ string_of_content node
      ^ string_of_children node ^ "</" ^ tag ^ ">"

let templates : (string, t) Hashtbl.t = Hashtbl.create 16

let template_exists name = Hashtbl.mem templates name

let get_template name = Hashtbl.find templates name

let set_template name tmpl = Hashtbl.replace templates name tmpl

(** Loading compiled templates *)
let loadfile filename = Dynlink.loadfile filename
