module Tag :
  sig
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
        | `MAIN
        | `META
        | `NAME of string
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
        | `RAW
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
        | `VAR of string
        | `VIDEO
        | `WBR ]
    val to_string : t -> string
    val nonclosing :
      [> `BR | `HR | `IMG | `INPUT | `LINK | `META | `WBR ] list
  end
type t = {
  tag : Tag.t;
  mutable attrs : (string * string) list;
  mutable content : string option;
  mutable children : t list;
}
val replace : t -> string -> t -> t
val tag :
  ?attr:(string * string) list ->
  ?content:string option -> ?children:t list -> Tag.t -> t
val var : string -> t
val import : string -> t
val html : ?attr:(string * string) list -> ?content:string -> t list -> t
val body : ?attr:(string * string) list -> ?content:string -> t list -> t
val head : ?attr:(string * string) list -> ?content:string -> t list -> t
val meta : (string * string) list -> t
val script : ?attr:(string * string) list -> string -> t
val canvas : ?content:string -> (string * string) list -> t
val style : ?attr:(string * string) list -> string -> t
val iframe : (string * string) list -> t
val div : ?attr:(string * string) list -> ?content:string -> t list -> t
val p : ?attr:(string * string) list -> ?children:t list -> string -> t
val span : ?attr:(string * string) list -> ?content:string -> t list -> t
val h1 : ?attr:(string * string) list -> ?children:t list -> string -> t
val h2 : ?attr:(string * string) list -> ?children:t list -> string -> t
val h3 : ?attr:(string * string) list -> ?children:t list -> string -> t
val h4 : ?attr:(string * string) list -> ?children:t list -> string -> t
val h5 : ?attr:(string * string) list -> ?children:t list -> string -> t
val h6 : ?attr:(string * string) list -> ?children:t list -> string -> t
val header : ?attr:(string * string) list -> ?content:string -> t list -> t
val main : ?attr:(string * string) list -> ?content:string -> t list -> t
val nav : ?attr:(string * string) list -> ?content:string -> t list -> t
val article : ?attr:(string * string) list -> ?content:string -> t list -> t
val section : ?attr:(string * string) list -> ?content:string -> t list -> t
val aside : ?attr:(string * string) list -> ?content:string -> t list -> t
val footer : ?attr:(string * string) list -> ?content:string -> t list -> t
val ul : ?attr:(string * string) list -> ?content:string -> t list -> t
val ol : ?attr:(string * string) list -> ?content:string -> t list -> t
val li : ?attr:(string * string) list -> ?content:string -> t list -> t
val text : ?attr:(string * string) list -> string -> t
val label : ?attr:(string * string) list -> string -> t
val link : ?children:t list -> ?content:string -> (string * string) list -> t
val a : ?children:t list -> (string * string) list -> string -> t
val img : (string * string) list -> t
val textarea : ?attr:(string * string) list -> string -> t
val input : (string * string) list -> t
val select : ?attr:(string * string) list -> t list -> t
val opt : ?attr:(string * string) list -> string -> t
val form : ?attr:(string * string) list -> t list -> t
val mk_form :
  ?attr:(string * string) list -> (string * string) list list -> string -> t
val audio : ?attr:(string * string) list -> ?content:string -> t list -> t
val video : ?attr:(string * string) list -> ?content:string -> t list -> t
val source : (string * string) list -> t
val title : string -> t
val inline : string -> t
val string_of_attrs : t -> string
val string_of_content : t -> string
val string_of_children : t -> string
val to_string : t -> string
val templates : (string, t) Hashtbl.t
val template_exists : string -> bool
val get_template : string -> t
val set_template : string -> t -> unit
val loadfile : string -> unit
