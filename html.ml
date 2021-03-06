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

(* Strings used in tagname and attribute names are all made lowercase (for easier pattern matching) *)
type attr = (string * string)
type tag = (string * attr list * html)
and html_chunk = Raw of string   (* copied verbatim *)
               | CData of string (* html entities replaced *)
               | Tag of tag      (* an element *)
               | Block of html   (* for commodity, will be inlined *)
and html = html_chunk list

(* {1 Helper for attribute list (structurally an alist)} *)

module AList = struct
    let mem al ?value_match name =
        List.exists (fun (n,v) ->
            n = name && match value_match with None -> true | Some f -> f v) al

    let find al name = List.assoc name al

    let filter_out names al =
        let rec aux res = function
            | [] -> List.rev res
            | (n,_ as x)::rest ->
                aux (if List.mem n names then res else x::res) rest
        in aux [] al

    let starts_with a b = String.starts_with b a

    let has_word w =
        let re = Str.regexp @@ "\\b"^ Str.quote w ^"\\b" in
        fun s -> try Str.search_forward re s 0 |> ignore ; true
                 with Not_found -> false

end



(** {1 Helpers to build simple docs} *)

(** {2 Main} *)
let tag name ?(attrs=[]) ?id ?cls content =
    let lc = String.lowercase in
    let attrs = List.map (fun (n,v) -> lc n, v) attrs in
    let attrs = match id with None -> attrs | Some str -> ("id", str)::attrs in
    let attrs = match cls with None -> attrs | Some str -> ("class", str)::attrs in
    let name = lc name in
    Tag (name, attrs, content)

let cdata txt = CData txt
let raw txt = Raw txt
let p = tag "p"
let h1 ?attrs ?id txt = tag "h1" ?attrs ?id [ cdata txt ]
let h2 ?attrs ?id txt = tag "h2" ?attrs ?id [ cdata txt ]
let h3 ?attrs ?id txt = tag "h3" ?attrs ?id [ cdata txt ]
let h4 ?attrs ?id txt = tag "h4" ?attrs ?id [ cdata txt ]
let em ?attrs ?id txt  = tag "em" ?attrs ?id [ cdata txt ]
let emph = em
let bold ?attrs ?id txt = tag "b" ?attrs ?id [ cdata txt ]
let div = tag "div"
let input ?id ?cls attrs = tag "input" ~attrs ?id ?cls []
let input_tnv ?id ?cls ?(attrs=[]) typ name value =
    input ?id ?cls (["type",typ; "name",name; "value",value] @ attrs)
let input_checkbox ?id ?cls ?attrs name value = input_tnv ?id ?cls ?attrs "checkbox" name value
let input_radio ?id ?cls ?attrs name value = input_tnv ?id ?cls ?attrs "radio" name value
let input_text ?id ?cls ?attrs name value = input_tnv ?id ?cls ?attrs "text" name value
let input_passwd ?id ?cls ?attrs name value = input_tnv ?id ?cls ?attrs "password" name value
let input_submit ?id ?cls ?attrs name value = input_tnv ?id ?cls ?attrs "submit" name value
let label ?id ?cls ?attrs = tag "label" ?attrs ?id ?cls
let button ?id ?cls ?attrs = tag "button" ?attrs ?id ?cls
let hidden ?attrs ?id name value =
    input ?id ((BatOption.default [] attrs) @ [ "type", "hidden"; "name", name; "value", value ])
let form ?(hiddens=[]) ?attrs ?id ?cls content =
    let h = List.map (fun (name, value) -> hidden name value) hiddens in
    tag "form" ?attrs ?id ?cls (h @ content)
let title ?attrs ?id txt = tag "title" ?attrs ?id [ cdata txt ]
let link_css url = tag "link" ~attrs:["rel","stylesheet"; "type","text/css"; "href",url] []
let a ?alt href content =
    let attrs = match alt with None -> [] | Some alt -> ["alt",alt] in
    let attrs = ("href",href)::attrs in
    tag ~attrs "a" content
let default_html_attrs = [ "xmlns", "http://www.w3.org/1999/xhtml" ; "xml:lang", "en" ]
let html ?(attrs=default_html_attrs) ?onload head body =
    tag "html" ~attrs [
      tag "head" head ;
      tag "body" ?attrs:(Option.map (fun x -> ["onload", x]) onload) body ]
let span = tag "span"
let pre = tag "pre"
let ul = tag "ul"
let ol = tag "ol"
let li = tag "li"

(** {2 Tables} *)
let td = tag "td"
let th = tag "th"
let tr = tag "tr"
let thead = tag "thead"
let tbody = tag "tbody"
let table = tag "table"
let img ?attrs ?id src =
  tag "img" ~attrs:(("src", src)::(Option.default [] attrs)) ?id []

(** {1 Printers} *)

let print_char = Char.print
let print_string = String.print

let print_cdata oc s =
    String.iter (function
        | '&' -> print_string oc "&and;"
        | '<' -> print_string oc "&lt;"
        | '>' -> print_string oc "&gt;"
        | '\'' -> print_string oc "&apos;"
        | '"' -> print_string oc "&quot;"
        (* etc *)
        | c -> print_char oc c) s

let print_attr oc (name, value) =
    print_string oc name ;
    print_string oc "=\"" ;
    print_string oc (String.nreplace value "\"" "&quot;") ;
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
    | Block h -> List.iter (print oc) h

let print_xml_head oc =
    Printf.fprintf oc "<!DOCTYPE html\n          PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"

let comment txt =
    raw ("\n<!-- " ^ txt ^ "-->\n")

(** {1 Javascript} *)

let script ?(attrs=[]) str = tag "script" ~attrs:(("type","text/javascript")::attrs) [ raw str ]

module JSValue = struct
    type t = String of string
           | Num of float
           | Int of int
           | Bool of bool
           | List of t list

    let rec print fmt = function
        | String s -> Printf.fprintf fmt "'%s'" (String.nreplace s "'" "\\'")
        | Num f -> Printf.fprintf fmt "%g" f
        | Int i -> Printf.fprintf fmt "%d" i
        | Bool b -> Printf.fprintf fmt (if b then "true" else "false")
        | List l -> List.print ~first:"[" ~last:"]" ~sep:"," print fmt l
end

let json d =
    IO.to_string
        (List.print ~first:"{" ~last:"}" ~sep:","
            (Tuple2.print ~first:"" ~last:"" ~sep:":" String.print JSValue.print)) d |>
    raw

(** {1 SVG} *)

let rec my_string_of_float f =
    if f = 0. then "0" else  (* take good care of ~-.0. *)
    if f < 0. then "-"^ my_string_of_float (~-.f) else
    (* SVG don't like digits ending with a dot *)
    let s = Printf.sprintf "%.5f" f in (* limit number of significant digits to reduce page size *)
    (* chop trailing zeros and trailing dot *)
    let rec chop last l =
      let c = s.[l] in
      if last || l < 1 || c <> '0' && c <> '.' then (
        if l = String.length s - 1 then s else
        String.sub s 0 (l + 1)
      ) else
        chop (c = '.') (l - 1) in
    chop false (String.length s - 1)

let rec add_attrs attrs = function
    | [] -> attrs
    | (_, None) :: l -> add_attrs attrs l
    | (name, Some v) :: l -> add_attrs ((name, v)::attrs) l

let svg ?(attrs=[]) ?width ?height =
    let attrs = add_attrs attrs
                          [ "width", Option.map my_string_of_float width ;
                            "height", Option.map my_string_of_float height ] in
    tag "svg" ~attrs

let g = tag "g"

let rect ?(attrs=[]) ?id ?cls ?fill ?stroke ?stroke_opacity ?stroke_dasharray ?fill_opacity ?stroke_width x y width height =
    let attrs = add_attrs (attrs @
                          [ "x", my_string_of_float x ;
                            "y", my_string_of_float y ;
                            "width", my_string_of_float width ;
                            "height", my_string_of_float height ])
                          [ "fill", fill ;
                            "stroke-opacity", Option.map my_string_of_float stroke_opacity ;
                            "stroke-dasharray", stroke_dasharray ;
                            "fill-opacity", Option.map my_string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map my_string_of_float stroke_width ] in
    tag "rect" ~attrs ?id ?cls []

let circle ?(attrs=[]) ?id ?cls ?cx ?cy ?fill ?stroke ?stroke_opacity ?stroke_dasharray ?fill_opacity ?stroke_width r =
    let attrs = add_attrs (("r", my_string_of_float r) :: attrs)
                          [ "cx", Option.map my_string_of_float cx ;
                            "cy", Option.map my_string_of_float cy ;
                            "fill", fill ;
                            "stroke-opacity", Option.map my_string_of_float stroke_opacity ;
                            "stroke-dasharray", stroke_dasharray ;
                            "fill-opacity", Option.map my_string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map my_string_of_float stroke_width ] in
    tag "circle" ~attrs ?id ?cls []

let text ?(attrs=[]) ?id ?cls ?x ?y ?dx ?dy ?style ?rotate ?text_length ?length_adjust ?font_family ?font_size ?fill ?stroke ?stroke_width ?stroke_opacity ?stroke_dasharray ?fill_opacity txt =
    let attrs = add_attrs attrs
                          [ "x",  Option.map my_string_of_float x ;
                            "y",  Option.map my_string_of_float y ;
                            "dx", Option.map my_string_of_float dx ;
                            "dy", Option.map my_string_of_float dy ;
                            "style", style ;
                            "rotate", Option.map my_string_of_float rotate ;
                            "textLength", Option.map my_string_of_float text_length ;
                            "lengthAdjust", Option.map my_string_of_float length_adjust ;
                            "font-family", font_family ;
                            "font-size", Option.map my_string_of_float font_size ;
                            "fill", fill ;
                            "stroke-opacity", Option.map my_string_of_float stroke_opacity ;
                            "stroke-dasharray", stroke_dasharray ;
                            "fill-opacity", Option.map my_string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map my_string_of_float stroke_width ] in
    tag "text" ~attrs ?id ?cls [ raw txt ]

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

let path ?(attrs=[]) ?id ?cls ?style ?transform ?fill ?stroke ?stroke_width ?stroke_opacity ?stroke_dasharray ?fill_opacity d =
    let attrs = add_attrs (("d", d) :: attrs)
                          [ "style", style ;
                            "transform", transform ;
                            "fill", fill ;
                            "stroke-opacity", Option.map my_string_of_float stroke_opacity ;
                            "stroke-dasharray", stroke_dasharray ;
                            "fill-opacity", Option.map my_string_of_float fill_opacity ;
                            "stroke", stroke ;
                            "stroke-width", Option.map my_string_of_float stroke_width ] in
    tag "path" ~attrs ?id ?cls []

let moveto (x, y) = "M "^my_string_of_float x^" "^my_string_of_float y^" "
let lineto (x, y) = "L "^my_string_of_float x^" "^my_string_of_float y^" "
let curveto (x1, y1) (x2, y2) (x, y) =
    "C "^my_string_of_float x1^" "^my_string_of_float y1^" "^
         my_string_of_float x2^" "^my_string_of_float y2^" "^
         my_string_of_float x ^" "^my_string_of_float y ^" "
let smoothto (x2, y2) (x, y) =
    "S "^my_string_of_float x2^" "^my_string_of_float y2^" "^
         my_string_of_float x ^" "^my_string_of_float y ^" "
let closepath = "Z"

let line ?(attrs=[]) ?id ?cls ?style ?stroke ?stroke_width ?stroke_opacity ?stroke_dasharray (x1, y1) (x2, y2) =
    let attrs = add_attrs ([ "x1", my_string_of_float x1 ;
                             "y1", my_string_of_float y1 ;
                             "x2", my_string_of_float x2 ;
                             "y2", my_string_of_float y2 ] @ attrs)
                          [ "style", style ;
                            "stroke-opacity", Option.map my_string_of_float stroke_opacity ;
                            "stroke-dasharray", stroke_dasharray ;
                            "stroke", stroke ;
                            "stroke-width", Option.map my_string_of_float stroke_width ] in
    tag "line" ~attrs ?id ?cls []

