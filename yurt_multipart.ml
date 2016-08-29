exception Invalid_multipart_form

(** The multipart Yurt_multipart module exists because Cohttp lacks server-side
 * support for parsing multipart form requests. This module may reject your legally
 * formatted requests, but I will do my best to fix all the edge cases as they come up.
 *
 * There are a couple of big things from RFC2388 that aren't implemented yet:
 *    1. multipart/mixed type multiparts are not completely parsed.
 *    2. content-transfer-encoding is currently ignored.
 *    3.*)

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

let parse_form_multipart (req: Yurt_request_ctx.request_context) : multipart list =
    (** Output *)
    let out = ref [] in
    let content_type = Yurt_util.unwrap_option_default (Yurt_hdr.get req "Content-Type") "" in
    let b = split_semicolon content_type in
    let boundary = match b with
        | x::y::[] -> String.sub y 9 (String.length y - 9)
        | _ -> raise Invalid_multipart_form in
    let boundary_a = "--" ^ boundary in
    let boundary_b = boundary_a ^ "--" in

    (* Input lines *)
    let lines = Str.split line_regexp (Yurt_request_ctx.body_string req) in

    (* Current multipart context *)
    let current = ref {data = ""; attr = Hashtbl.create 16; name = ""} in

    (* Body buffer *)
    let buffer = Buffer.create 512 in

    (* True when the parser is in a header section *)
    let in_header = ref false in

    let _ = List.iter (fun line ->
        match line with
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
            Buffer.add_string buffer "\r\n") lines in !out

let parse_form_multipart_lwt (req : Yurt_request_ctx.request_context) : multipart list Lwt.t =
   Lwt.return (parse_form_multipart req)
