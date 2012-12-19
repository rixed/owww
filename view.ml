open Html

let page head_title body_content =
    html [ tag "title" [cdata head_title] ;
           tag "link" ~attrs:["rel", "stylesheet" ; "type", "text/css"; "media", "screen"; "href", "style.css" ] [] ]
         [ tag "body" body_content ]

let err content =
    page "OMG!" content

let backtrace e =
    [ div ~attrs:["class","backtrace"]
        [ h1 ("Exception: " ^ Printexc.to_string e) ;
          pre [ cdata (Printexc.get_backtrace ()) ] ] ]
