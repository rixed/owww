open Batteries

(* Debug (TODO: move away!) *)

let debug_msgs = ref []
let debug_msg s =
    debug_msgs := s :: !debug_msgs

(* Dispatch according to URL *)

(* TODO: a dedicated hash type for args, with an indication where we got the entry from? *)

let content_type = ref "text/html"
let set_cookies = ref []
let add_cookie n v =
    set_cookies := (n,v) :: !set_cookies

let run d =
    let args =
        let h = Hashtbl.create 17
        and all_args = Cgi.parse_args () @
                       Cgi.parse_cookies () in
        List.iter (fun (n,v) -> Hashtbl.add h n v) all_args ;
        h in
    let action =
        try Hashtbl.find args "action" |>
            String.nsplit ~by:"/"
        with Not_found -> ["main"] in
    let runner = try d action
                 with _ -> Ctrl.Invalid.run in
    let doc = try runner args
              with e -> View.err (View.backtrace e) in
    Cgi.header ~content_type:!content_type ~cookies:!set_cookies () ;
    Html.print stdout doc ;
    Printf.printf "\n<!-- OCAMLRUNPARAM: %s\nURL: %s\nPATH_INFO: %a\nARGS: %a\n Debug:\n %a ->\n"
        (try Sys.getenv "OCAMLRUNPARAM" with Not_found -> "unset")
        (Cgi.this_url ())
        (List.print String.print) Cgi.path_info
        (Hashtbl.print String.print String.print) args
        (List.print String.print) !debug_msgs

