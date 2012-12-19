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
and html_chunk = Raw of string | CData of string | Tag of tag
and html = html_chunk list

(** {1 Helpers to build simple docs} *)

(** {2 Main} *)
let tag name ?(attrs=[]) ?id content =
    Tag (name, (match id with None -> attrs | Some str -> ("id",str)::attrs), content)

let cdata txt = CData txt
let raw txt = Raw txt
let p = tag "p"
let h1 ?attrs ?id txt = tag "h1" ?attrs ?id [ cdata txt ]
let h2 ?attrs ?id txt = tag "h2" ?attrs ?id [ cdata txt ]
let h3 ?attrs ?id txt = tag "h3" ?attrs ?id [ cdata txt ]
let h4 ?attrs ?id txt = tag "h4" ?attrs ?id [ cdata txt ]
let div = tag "div"
let input attrs = tag "input" ~attrs []
let form action ?attrs ?id content =
    let hidden = input [ "type", "hidden" ;
                         "name", "action" ;
                         "value", action ] in
    tag "form" ?attrs ?id (hidden :: content)
let title ?attrs ?id txt = tag "title" ?attrs ?id [ cdata txt ]
let link_css url = tag "link" ~attrs:["rel","stylesheet"; "type","text/css"; "href",url] []
let default_html_attrs = [ "xmlns", "http://www.w3.org/1999/xhtml" ; "xml:lang", "en" ]
let html ?(attrs=default_html_attrs) head body =
    tag "html" ~attrs [ tag "head" head ; tag "body" body ]
let script str = tag "script" ~attrs:["type","text/javascript"] [ raw str ]
let span = tag "span"
let pre = tag "pre"

(** {2 Tables} *)
let td = tag "td"
let th = tag "th"
let tr = tag "tr"
let thead = tag "thead"
let tbody = tag "tbody"
let table = tag "table"

(** {1 Printers} *)

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
    | Raw s -> print_string oc s
    | Tag t -> print_tag oc t

let print_xml_head oc =
    Printf.fprintf oc "<!DOCTYPE html\n          PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"

let comment txt =
    raw ("\n<!-- " ^ txt ^ "-->\n")

(** {1 SVG} *)

let string_of_float f =
    (* SVG don't like digits ending with a dot *)
    let s = Printf.sprintf "%5f" f in (* limit number of significant digits to reduce page size *)
    if s.[String.length s-1] = '.' then s ^ "0" else s

let rec add_attrs attrs = function
    | [] -> attrs
    | (_, None) :: l -> add_attrs attrs l
    | (name, Some v) :: l -> add_attrs ((name, v)::attrs) l

let svg ?(attrs=[]) ?width ?height =
    let attrs = add_attrs attrs
                          [ "width", Option.map string_of_float width ;
                            "height", Option.map string_of_float height ] in
    tag "svg" ~attrs

let g = tag "g"

let rect ?(attrs=[]) ?fill ?stroke ?stroke_opacity ?fill_opacity ?stroke_width x y width height =
    let attrs = add_attrs (attrs @
                          [ "x", string_of_float x ;
                            "y", string_of_float y ;
                            "width", string_of_float width ;
                            "height", string_of_float height ])
                          [ "fill", fill ;
                            "stroke-opacity", Option.map string_of_float stroke_opacity ;
                            "fill-opacity", Option.map string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map string_of_float stroke_width ] in
    tag "rect" ~attrs []

let circle ?(attrs=[]) ?cx ?cy ?fill ?stroke ?stroke_opacity ?fill_opacity ?stroke_width r =
    let attrs = add_attrs (("r", string_of_float r) :: attrs)
                          [ "cx", Option.map string_of_float cx ;
                            "cy", Option.map string_of_float cy ;
                            "fill", fill ;
                            "stroke-opacity", Option.map string_of_float stroke_opacity ;
                            "fill-opacity", Option.map string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map string_of_float stroke_width ] in
    tag "circle" ~attrs []

let text ?(attrs=[]) ?id ?x ?y ?dx ?dy ?style ?rotate ?text_length ?length_adjust ?font_family ?font_size ?fill ?stroke ?stroke_width ?stroke_opacity ?fill_opacity txt =
    let attrs = add_attrs attrs
                          [ "x",  Option.map string_of_float x ;
                            "y",  Option.map string_of_float y ;
                            "dx", Option.map string_of_float dx ;
                            "dy", Option.map string_of_float dy ;
                            "style", style ;
                            "rotate", Option.map string_of_float rotate ;
                            "textLength", Option.map string_of_float text_length ;
                            "lengthAdjust", Option.map string_of_float length_adjust ;
                            "font-family", font_family ;
                            "font-size", Option.map string_of_float font_size ;
                            "fill", fill ;
                            "stroke-opacity", Option.map string_of_float stroke_opacity ;
                            "fill-opacity", Option.map string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map string_of_float stroke_width ] in
    tag "text" ~attrs ?id [ raw txt ]

(* Takes a list of (string * font_size) *)
let texts ?attrs ?id ?dx ?dy ?style ?rotate ?text_length ?length_adjust ?font_family ?fill ?stroke ?stroke_width ?stroke_opacity ?fill_opacity x y txts =
    let rec aux res y = function
    | [] -> res
    | (str, sz)::txts' ->
        aux ((text ?attrs ?id ~x ~y ~font_size:sz
                   ?dx ?dy ?style ?rotate ?text_length ?length_adjust
                   ?font_family ?fill ?stroke ?stroke_width ?stroke_opacity ?fill_opacity
                   str)::res)
            (y +. sz *. 1.05) txts' in
    List.rev (aux [] y txts)

let path ?(attrs=[]) ?style ?transform ?fill ?stroke ?stroke_width ?stroke_opacity ?fill_opacity d =
    let attrs = add_attrs (("d", d) :: attrs)
                          [ "style", style ;
                            "transform", transform ;
                            "fill", fill ;
                            "stroke-opacity", Option.map string_of_float stroke_opacity ;
                            "fill-opacity", Option.map string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map string_of_float stroke_width ] in
    tag "path" ~attrs []

let moveto (x, y) = "M "^string_of_float x^" "^string_of_float y^" "
let lineto (x, y) = "L "^string_of_float x^" "^string_of_float y^" "
let curveto (x1, y1) (x2, y2) (x, y) =
    "C "^string_of_float x1^" "^string_of_float y1^" "^
         string_of_float x2^" "^string_of_float y2^" "^
         string_of_float x ^" "^string_of_float y ^" "
let smoothto (x2, y2) (x, y) =
    "S "^string_of_float x2^" "^string_of_float y2^" "^
         string_of_float x ^" "^string_of_float y ^" "
let closepath = "Z"

let line ?(attrs=[]) ?style ?stroke ?stroke_width ?stroke_opacity (x1, y1) (x2, y2) =
    let attrs = add_attrs ([ "x1", string_of_float x1 ;
                             "y1", string_of_float y1 ;
                             "x2", string_of_float x2 ;
                             "y2", string_of_float y2 ] @ attrs)
                          [ "style", style ;
                            "stroke-opacity", Option.map string_of_float stroke_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map string_of_float stroke_width ] in
    tag "line" ~attrs []

