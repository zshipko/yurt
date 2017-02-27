open Lwt
open Yurt_route

module Body = Cohttp_lwt_body
module Request = Cohttp_lwt_unix.Request
module Response = Cohttp.Response
module Header = Cohttp.Header

type status_code = Cohttp.Code.status_code

(** Response type *)
and response = (Response.t * Body.t) Lwt.t

(** HTTP handler *)
and endpoint = Request.t -> Yurt_route.params -> Body.t -> response

let query_all (req : Request.t) : (string * string list) list =
    Uri.query (Request.uri req)

let query_dict_of_query (q : (string * string list) list) =
    let d = Hashtbl.create 16 in
    List.iter (fun (k, v) ->
        if Hashtbl.mem d k then
            let l = Hashtbl.find d k in
            Hashtbl.replace d k (l @ v)
        else Hashtbl.replace d k v) q; d

(** Get a hashtable of all query string parameters *)
let query req : (string, string list) Hashtbl.t =
    query_dict_of_query (query_all req)

(** Convert all query string paramters to a json object *)
let query_json req : Ezjsonm.t =
    let f = query_all req in
    `O (List.map (fun (k, v) ->
        if List.length v = 1 then
            k, Ezjsonm.encode_string (List.nth v 0)
        else
            k, `A (List.map Ezjsonm.encode_string v)) f)

(** Get a string value for a single query string value by key *)
let query_string req (name : string) : string option =
    Uri.get_query_param (Request.uri req) name

(** Get an int value for a single query string value by key *)
let query_int req (name : string) : int option =
    let qs = query_string req name in
    match qs with
    | Some s ->
        begin try Some (int_of_string s)
            with _ -> None end
    | None -> None

(** Get an float value for a single query string value by key*)
let query_float req (name : string) : float option =
    let qe = query_string req name in
    match qe with
    | Some s ->
        begin try Some (float_of_string s)
            with _ -> None end
    | None -> None
