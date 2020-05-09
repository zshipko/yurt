open Lwt.Infix
module Request = Cohttp_lwt_unix.Request
module Response = Cohttp.Response
module Header = Cohttp.Header

module Body = struct
  include Cohttp_lwt.Body

  type transfer_encoding = Cohttp.Transfer.encoding

  let to_string = Cohttp_lwt.Body.to_string

  let to_stream = Cohttp_lwt.Body.to_stream

  let to_json body =
    Cohttp_lwt.Body.to_string body >|= fun s -> Ezjsonm.from_string s

  let of_string = Cohttp_lwt.Body.of_string

  let of_stream = Cohttp_lwt.Body.of_stream

  let of_json j = Ezjsonm.to_string j |> Cohttp_lwt.Body.of_string

  let map = Cohttp_lwt.Body.map

  let length = Cohttp_lwt.Body.length

  let is_empty = Cohttp_lwt.Body.is_empty

  let drain = Cohttp_lwt.Body.drain_body

  let transfer_encoding = Cohttp_lwt.Body.transfer_encoding
end

type status_code = Cohttp.Code.status_code
(** Response status code *)

and response = (Response.t * Body.t) Lwt.t
(** Response type *)

and endpoint = Request.t -> Yurt_route.params -> Body.t -> response
(** HTTP handler *)

module Query = struct
  type t = (string, string list) Hashtbl.t

  let query_all (req : Request.t) : (string * string list) list =
    Uri.query (Request.uri req)

  let query_dict_of_query (q : (string * string list) list) =
    let d = Hashtbl.create 16 in
    List.iter
      (fun (k, v) ->
        if Hashtbl.mem d k then
          let l = Hashtbl.find d k in
          Hashtbl.replace d k (l @ v)
        else Hashtbl.replace d k v)
      q;
    d

  (** Get a hashtable of all query string parameters *)
  let get req : (string, string list) Hashtbl.t =
    query_dict_of_query (query_all req)

  (** Convert all query string paramters to a json object *)
  let to_json req : Ezjsonm.t =
    let f = query_all req in
    `O
      (List.map
         (fun (k, v) ->
           if List.length v = 1 then (k, Ezjsonm.encode_string (List.nth v 0))
           else (k, `A (List.map Ezjsonm.encode_string v)))
         f)

  (** Get a string value for a single query string value by key *)
  let string req (name : string) : string option =
    Uri.get_query_param (Request.uri req) name

  (** Get an int value for a single query string value by key *)
  let int req (name : string) : int option =
    let qs = string req name in
    match qs with
    | Some s -> ( try Some (int_of_string s) with _ -> None )
    | None -> None

  (** Get a float value for a single query string value by key*)
  let float req (name : string) : float option =
    let qe = string req name in
    match qe with
    | Some s -> ( try Some (float_of_string s) with _ -> None )
    | None -> None

  (** Get a json value for a single query string value by key*)
  let json req (name : string) =
    let qe = string req name in
    match qe with
    | Some s -> ( try Some (Ezjsonm.from_string s) with _ -> None )
    | None -> None
end
