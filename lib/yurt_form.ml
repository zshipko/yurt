
open Lwt
open Yurt_request_ctx
open Cohttp_lwt_unix

exception Invalid_multipart_form

(** Parse URL encoded form *)
let urlencoded body : (string, string list) Hashtbl.t Lwt.t =
    let dst = Hashtbl.create 16 in
    Body.to_string body
    >|= Uri.query_of_encoded
    >|= Lwt_list.iter_s (fun (k, v) ->
        Lwt.return (
            if Hashtbl.mem dst k then
            let l = Hashtbl.find dst k in
            Hashtbl.replace dst k (l @ v)
        else
            Hashtbl.replace dst k v))
    >>= (fun _ -> Lwt.return dst)

let urlencoded_list body : (string * string list) list Lwt.t =
    Body.to_string body
    >|= Uri.query_of_encoded

(** Parse URL encoded form into JSON *)
let urlencoded_json body : Ezjsonm.t Lwt.t =
    urlencoded_list body
    >|= fun f ->
        `O (List.map (fun (k, v) ->
            k, `A (List.map Ezjsonm.encode_string v)) f)

(** There are a couple of big things from RFC2388 that aren't implemented yet:
 *    1. multipart/mixed content type may not be parsed correctly.
 *    2. content-transfer-encoding is currently ignored. *)

type multipart = {
    mutable data : char Lwt_stream.t;
    mutable name : string;
    attr : (string, string list) Hashtbl.t
}

let line_regexp = Str.regexp "\r\n"
let equal_regexp = Str.regexp "="
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
    let content_type = Yurt_util.unwrap_option_default (Header.get req.Request.headers "Content-Type") "" in
    Str.string_match (is_multipart_regexp) content_type 0

let multipart req body : multipart list Lwt.t =
    (** Output *)
    let out = ref [] in

    let content_type = Yurt_util.unwrap_option_default (Header.get req.Request.headers "Content-Type") "" in

    let b = split_semicolon content_type in
    let boundary = match b with
        | x::y::[] -> String.sub y 9 (String.length y - 9)
        | _ -> raise Invalid_multipart_form in
    let boundary_a = "--" ^ boundary in
    let boundary_b = boundary_a ^ "--" in

    (* Current multipart context *)
    let current = ref {data = Lwt_stream.of_string ""; attr = Hashtbl.create 16; name = ""} in

    (* Body buffer *)
    let buffer = Buffer.create 512 in

    (* True when the parser is in a header section *)
    let in_header = ref false in

    (* Input lines *)
    Body.to_string body

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
                let b = Buffer.sub buffer 0 (bl - 2) in
                let _ = !current.data <- Lwt_stream.of_string b in
                let _ = Buffer.reset buffer in
                let _ = out := !out @ [c] in
                current := {data = Lwt_stream.of_string ""; attr = Hashtbl.create 16; name = ""}

        (* End of header *)
        | x when !in_header && x="" ->
            in_header := false

        (* Get attributes  *)
        | x when !in_header ->
            let m = "Content-Disposition: form-data; " in
            let mlen = String.length m in
            if String.length x >= String.length m && String.sub x 0 mlen = m then
                let x = String.sub x mlen (String.length x - String.length m) in
                let parts = split_semicolon x in
                List.iter (fun part ->
                    let p = Str.split equal_regexp part in
                    let k = String.trim (List.hd p) in
                    let v = List.tl p |> String.concat "=" in
                    let v = String.sub v 1 (String.length v - 2) in
                    if k == "name" then
                        !current.name <- v
                    else
                        if Hashtbl.mem !current.attr k then
                            let dst = Hashtbl.find !current.attr k in
                            Hashtbl.replace !current.attr k (dst @ [v])
                        else
                            Hashtbl.replace !current.attr k [v]) parts

        (* In body *)
        | x ->
            Buffer.add_string buffer x;
            Buffer.add_string buffer "\r\n" in Lwt.return_unit)
    >>= (fun _ -> Lwt.return !out)

type form =
    | Multipart of multipart list
    | Urlencoded of (string, string list) Hashtbl.t

(** Parse URL encoded form *)
let parse_form req body : form Lwt.t =
    if is_multipart req then
        multipart req body
        >|= (fun f -> Multipart f)
    else
        urlencoded body
        >|= (fun f -> Urlencoded f)
