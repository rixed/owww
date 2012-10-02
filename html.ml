(*
  Unsophisticated HTML generation.
  Requirements:
  - We want an AST (for testing), the simpler the better
  - We do not want to build the string (ie. straight from AST to stdout)
  - We do not want to enforce XHTML validity

  So our AST is build from mere (lowercase) strings for tagname and attributes
  names/values (a la sxml).
*)
open Batteries

type attr = (string * string)
type tag = (string * attr list * html)
and html_chunk = CData of string | Tag of tag
and html = html_chunk list

(* {1 Helpers to build simple docs} *)

(* {2 Main} *)
let tag name ?(attrs=[]) content = Tag (name, attrs, content)
let cdata txt = CData txt
let p = tag "p"
let h1 ?attrs txt = tag "h1" ?attrs [ cdata txt ]
let h2 ?attrs txt = tag "h2" ?attrs [ cdata txt ]
let h3 ?attrs txt = tag "h3" ?attrs [ cdata txt ]
let div = tag "div"
let input attrs = tag "input" ~attrs []
let form action ?attrs content =
    let hidden = input [ "type", "hidden" ;
                         "name", "action" ;
                         "value", action ] in
    tag "form" ?attrs (hidden :: content)
let title ?attrs txt = tag "title" ?attrs [ cdata txt ]
let link_css url = tag "link" ~attrs:["rel","stylesheet"; "type","test/css"; "href",url] []
let default_html_attrs = [ "xmlns", "http://www.w3.org/1999/xhtml" ; "xml:lang", "en" ]
let html ?(attrs=default_html_attrs) head body =
    tag "html" ~attrs [ tag "head" head ; tag "body" body ]

(* {2 Tables} *)
let td = tag "td"
let th = tag "th"
let tr = tag "tr"
let table = tag "table"

(* {1 Printers} *)

let print_char = Char.print
let print_string = String.print

let print_cdata oc s =
    String.iter (function
        | '&' -> print_string oc "&and;"
        | '<' -> print_string oc "&lt;"
        | '>' -> print_string oc "&gt;"
        (* etc *)
        | c -> print_char oc c) s

let print_attr oc (name, value) =
    print_string oc name ;
    print_string oc "=\"" ;
    print_string oc value ;
    print_char oc '"'

let rec print_attrs oc = function
    | [] -> ()
    | attr::attrs' ->
        print_char oc ' ' ;
        print_attr oc attr ;
        print_attrs oc attrs'

let print_otag oc name attrs =
    print_char oc '<' ;
    print_string oc name ;
    print_attrs oc attrs ;
    print_char oc '>'

let print_octag oc name attrs =
    print_char oc '<' ;
    print_string oc name ;
    print_attrs oc attrs ;
    print_string oc "/>"

let print_ctag oc name =
    print_string oc "</" ;
    print_string oc name ;
    print_string oc ">\n"

let is_empty = function
    | "base" | "meta" | "link" | "hr" | "br" | "param" | "img"
    | "area" | "input" | "col" | "basefont" | "isindex" | "frame" -> true
    | _ -> false

let rec print_tag oc = function
    | name, attrs, [] when is_empty name ->
        print_octag oc name attrs
    | name, attrs, content ->
        print_otag oc name attrs ;
        List.iter (print oc) content ;
        print_ctag oc name

and print oc = function
    | CData s -> print_cdata oc s
    | Tag t -> print_tag oc t

let print_xml_head oc =
    Printf.fprintf oc "<!DOCTYPE html\n          PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"

