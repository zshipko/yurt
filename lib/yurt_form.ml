
open Lwt
open Yurt_request_ctx
open Cohttp_lwt_unix

exception Invalid_multipart_form

(** Parse URL encoded form *)
let parse_form_urlencoded (req : request_context) : (string, string list) Hashtbl.t Lwt.t =
    let dst = Hashtbl.create 16 in
    body_string req
    >|= Uri.query_of_encoded
    >|= Lwt_list.iter_s (fun (k, v) ->
        Lwt.return (if Hashtbl.mem dst k then
            let l = Hashtbl.find dst k in
            Hashtbl.replace dst k (l @ v)
        else
        Hashtbl.replace dst k v))
    >>= (fun _ -> Lwt.return dst)

let parse_form_urlencoded_list (req : request_context) : (string * string list) list Lwt.t =
    body_string req
    >|= Uri.query_of_encoded

(** Parse URL encoded form into a Merz.value *)
let parse_form_urlencoded_value (req : request_context) : Merz.value Lwt.t =
    parse_form_urlencoded req
    >|= (fun f ->
        `Dict (value_dict_of_query_dict f))

(** There are a couple of big things from RFC2388 that aren't implemented yet:
 *    1. multipart/mixed content type may not be parsed correctly.
 *    2. content-transfer-encoding is currently ignored. *)

type multipart = {
    mutable data : string;
    mutable name : string;
    attr : (string, string list) Hashtbl.t
}

let line_regexp = Str.regexp "\r\n"
let kv_regexp = Str.regexp "\\([^=]*\\)=\"\\([^;]*\\)\";?"
let semicolon_regexp = Str.regexp "; ?"

let split_semicolon (s : string) : string list =
    Str.split semicolon_regexp s

let get_attr (m : multipart) (attr : string) : string list =
    try Hashtbl.find m.attr attr
    with Not_found -> []

(** Return true when the multipart object has a filename attribute *)
let is_file (m : multipart) : bool =
    match get_attr m "filename" with
    | [] -> false
    | _ -> true

let is_multipart_regexp = Str.regexp "multipart/.*"

let is_multipart req : bool =
    let content_type = Yurt_util.unwrap_option_default (Yurt_header.get req "Content-Type") "" in
    Str.string_match (is_multipart_regexp) content_type 0

let parse_form_multipart (req: Yurt_request_ctx.request_context) : multipart list Lwt.t =
    (** Output *)
    let out = ref [] in

    let content_type = Yurt_util.unwrap_option_default (Yurt_header.get req "Content-Type") "" in

    let b = split_semicolon content_type in
    let boundary = match b with
        | x::y::[] -> String.sub y 9 (String.length y - 9)
        | _ -> raise Invalid_multipart_form in
    let boundary_a = "--" ^ boundary in
    let boundary_b = boundary_a ^ "--" in

    (* Current multipart context *)
    let current = ref {data = ""; attr = Hashtbl.create 16; name = ""} in

    (* Body buffer *)
    let buffer = Buffer.create 512 in

    (* True when the parser is in a header section *)
    let in_header = ref false in

    (* Input lines *)
    Yurt_request_ctx.body_string req

    >>= (fun s -> Lwt.return (Str.split line_regexp s))

    >|= Lwt_list.iter_s (fun line ->
        let _ = match line with
        (* Boundary *)
        | x when x = boundary || x = boundary_a  || x = boundary_b ->
            let c = !current in
            let _ = in_header := true in
            let bl = Buffer.length buffer in
            if bl > 0 ||
               Hashtbl.length c.attr > 0 then
                (** The new buffer contains an extra "\r\n" that needs to be removed *)
                let _ = c.data <- Buffer.sub buffer 0 (bl - 2) in
                let _ = Buffer.reset buffer in
                let _ = out := !out @ [c] in
                current := {data = ""; attr = Hashtbl.create 16; name = ""}

        (* End of header *)
        | x when !in_header && x = "" ->
            in_header := false

        (* Get attributes  *)
        | x when !in_header ->
            if try String.sub x 0 32 = "Content-Disposition: form-data; " with _ -> false then
                if Str.string_match kv_regexp x 31 then
                    let _end = Str.match_end () in
                    let rec _inner d i =
                        (* Attribute key *)
                        let k = Str.matched_group i x in

                        (* Attribute value *)
                        let v = Str.matched_group (i + 1) x in

                        (* Set name *)
                        if k = "name" then !current.name <- k;
                        try
                            let x = Hashtbl.find d k in
                            Hashtbl.replace d k (x @ [v])
                        with Not_found -> Hashtbl.replace d k [v];
                        if Str.group_end i < _end then
                            _inner d (i + 2)
                    in (try _inner !current.attr 1
                       with _ -> ())
        (* In body *)
        | x ->
            Buffer.add_string buffer x;
            Buffer.add_string buffer "\r\n" in Lwt.return_unit)
    >>= (fun _ -> Lwt.return !out)

type form =
    | Multipart of multipart list
    | Urlencoded of (string, string list) Hashtbl.t

(** Parse URL encoded form *)
let parse_form (req : request_context) : form Lwt.t =
    if is_multipart req then
        parse_form_multipart req
        >|= (fun f -> Multipart f)
    else
        parse_form_urlencoded req
        >|= (fun f -> Urlencoded f)
