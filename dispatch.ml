open Batteries

(* Debug (TODO: move away!) *)

let debug_msgs = ref []
let debug_msg s =
    debug_msgs := s :: !debug_msgs

(* Dispatch according to URL *)

(* TODO: a dedicated hash type for args, with an indication where we got the entry from? *)

let default_content_type = "application/xhtml+xml"
let content_type = ref default_content_type
let status = ref (200, "OK")
let current_cookies = ref []
let set_cookies = Hashtbl.create 11
let add_cookie n v =
    debug_msg (Printf.sprintf "Add cookie %s -> %s" n v) ;
    Hashtbl.replace set_cookies n v

(* Helpers to get a single argument (most current case) *)
exception MissingParameter of string
let single getter ?default n =
  match getter n with
  | [] -> (match default with
             None -> raise (MissingParameter n)
           | Some d -> d)
  | f :: _ -> f
let single_int getter ?default n =
  single getter ?default n |> int_of_string
let single_float getter ?default n =
  single getter ?default n |> float_of_string

let serve f =
    content_type :=
      (try Sys.getenv "CONTENT_TYPE"
       with Not_found -> default_content_type) ;
    let cgi_params = Cgi.parse_args () in
    let getter =
        current_cookies := Cgi.parse_cookies () ;
        let h = Hashtbl.create 17 in
        List.iter (fun (n,v) -> Hashtbl.add h n v)
            (cgi_params @ !current_cookies) ;
        Hashtbl.find_all h in
    (* body is whatever is in stdin, until either CONTENT_LENGTH or EOF *)
    let body = Cgi.read_body () in
    let doc = try f getter body
              with e ->
                  content_type := "text/html" ;
                  status := 500, "Uncaught Exception" ;
                  [ View.err (View.backtrace e) ] in
    let cookies = Hashtbl.fold (fun k v l -> (k,v)::l) set_cookies [] in
    Cgi.header ~status:(fst !status) ~err_msg:(snd !status) ~content_type:!content_type ~cookies () ;
    List.iter (Html.print stdout) doc ;
    if getter "debug" <> [] then
        Printf.printf "\n<!-- OCAMLRUNPARAM: %s\nURL: %s\nPATH_INFO: %a\nARGS: %a\n Debug:\n %a -->\n"
            (try Sys.getenv "OCAMLRUNPARAM" with Not_found -> "unset")
            (Cgi.this_url ())
            (List.print String.print) Cgi.path_info
            (List.print String.print) (List.map (fun (k,v) -> k^":"^v) cgi_params)
            (List.print ~sep:"<br/>\n" String.print) (List.rev !debug_msgs)

(* Serves according to a dispatch function on the value of "action" parameter *)
let run d =
    serve (fun getter _body ->
        let action =
            match getter "action" with
            | [a] -> String.nsplit ~by:"/" a
            | _   -> [] in
        let runner = try d action
                     with _ -> Ctrl.Invalid.run in
        runner getter)

