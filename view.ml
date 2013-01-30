open Html
open Batteries

let page head_title body_content =
    html [ tag "title" [cdata head_title] ;
           tag "link" ~attrs:["rel", "stylesheet" ; "type", "text/css"; "media", "screen"; "href", "style.css" ] [] ]
         [ tag "body" body_content ]

let err content =
    page "OMG!" (content @
        [ h2 "some infos that you may find useful" ;
          table [ tr [ th [ cdata "cwd" ] ; td [ cdata (Sys.getcwd ()) ] ] ;
                  tr [ th [ cdata "uid" ] ; td [ cdata (Unix.getuid () |> string_of_int) ] ] ;
                  tr [ th [ cdata "euid" ] ; td [ cdata (Unix.geteuid () |> string_of_int) ] ] ;
                  tr [ th [ cdata "gid" ] ; td [ cdata (Unix.getgid () |> string_of_int) ] ] ;
                  tr [ th [ cdata "egid" ] ; td [ cdata (Unix.getegid () |> string_of_int) ] ] ;
                  tr [ th [ cdata "groups" ] ; td [ cdata (IO.to_string (Array.print Int.print) (Unix.getgroups ())) ] ] ;
                  tr [ th [ cdata "login" ] ; td [ cdata (try Unix.getlogin () with _ -> "") ] ] ;
                  tr [ th [ cdata "time" ] ; td [ cdata (Unix.time () |> string_of_float) ] ] ] ])


let backtrace e =
    [ div ~attrs:["class","backtrace"]
        [ h1 ("Exception: " ^ Printexc.to_string e) ;
          pre [ cdata (Printexc.get_backtrace ()) ] ] ]
