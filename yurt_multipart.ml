exception Invalid_multipart_form
exception Stop_iteration

type multipart = {
    mutable data : string;
    mutable name : string;
    attr : (string, string list) Hashtbl.t
}

let split_semicolon (s : string) : string list =
    Str.split (Str.regexp "; ") s

let split_equal (s : string) : (string * string) =
    match Str.bounded_split (Str.regexp "=") s 2 with
    | [] -> ("", "")
    | a::[] -> (a, "")
    | a::b::_ -> (a, Qe_lexer.get_inside b 1)

let parse_multipart_form (req: Yurt_request_ctx.request) : multipart list =
    (** Output *)
    let out = ref [] in
    let content_type = Yurt_util.unwrap_option_default (Yurt_hdr.get req "Content-Type") "" in
    let b = split_semicolon content_type in
    let boundary = match b with
        | x::y::[] -> "--" ^ (String.sub y 9 (String.length y - 9))
        | _ -> raise Invalid_multipart_form in

    (* Input lines *)
    let lines = Str.split (Str.regexp "\r\n") (Yurt_request_ctx.string_of_body req) in

    let _ = Printf.printf "lines: %d\n" (List.length lines) in

    (* Current multipart context *)
    let current = ref {data = ""; attr = Hashtbl.create 16; name = ""} in

    (* True when the parser is in a header section *)
    let in_header = ref false in

    let add_current () =
        in_header := true;
        if !current.data <> "" || Hashtbl.length !current.attr > 0 then
            let _ = current := {data = ""; attr = Hashtbl.create 16; name = ""} in
            out := !out @ [!current] in

    try
    List.iter (fun line ->
        match line with
        (* Boundary *)
        | x when x = boundary || x = boundary ^ "--" ->
            print_endline "BOUNDARY";
            add_current ()

        (* End of header *)
        | x when !in_header && x = "" ->
            in_header := false

        (* Get attributes  *)
        | x when !in_header ->
            if String.sub x 0 32 = "Content-Disposition: form-data; " then
            if Str.string_match (Str.regexp "\\([^=]*\\)=\"\\([^;]*\\)\";?") x 31 then
                let i = ref 1 in
                try
                while true do
                    let k = Str.matched_group !i x in
                    let v = Str.matched_group (!i + 1) x in
                    if k = "name" then !current.name <- k;
                    print_endline ("ATTR: " ^ k ^ "=" ^ v);
                    if Hashtbl.mem !current.attr k then
                        let d = Hashtbl.find !current.attr k in
                        Hashtbl.replace !current.attr k (d @ [v])
                    else Hashtbl.replace !current.attr k [v];
                    i := !i + 2;
                done
                with _ -> ()
            else
                print_endline ("HEADER: " ^ x)

        (* In body *)
        | x ->
            print_endline ("BODY: " ^ x);
            !current.data <- !current.data ^ x) lines; !out
    with Stop_iteration ->
        if !current.data <> "" || Hashtbl.length !current.attr > 0 then
            !out @ [!current ]
        else !out
