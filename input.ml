open Batteries
open Html

(** {1 Simple datatypes that can be shared with the user} *)

(** The types and utilities you are likely to want to open *)
module Ops = struct

    exception InputError of string

    (** The type of an optional user input *)
    module type TYPE =
    sig
        (** The internal type for these values *)
        type t

        val to_edit : string -> (string -> string list) -> html
        val from : string -> (string -> string list) -> t
        (** or raise a exception *)
    end

    (** The type of an aggregate of user inputs, the difference being that
     * there are no erroneous nor unset values *)
    module type AGGR_TYPE =
    sig
        (** The internal type for these values *)
        type t

        val to_edit : string -> (string -> string list) -> html
        val from : string -> (string -> string list) -> t
    end

    let input_text_of name getter = match getter name with [] -> "" | x::_ -> x
    let input_error str = raise (InputError str)
    let missing_field_str = "this field is mandatory"
    let missing_field () = input_error missing_field_str
    let encode_cookie = Base64.str_encode
    let decode_cookie = Base64.str_decode
end

open Ops

module type LIMIT =
sig
    val min : int option
    val max : int option
end

module NoLimit : LIMIT =
struct
    let min = None
    let max = None
end

(* Implementation for an integer *)
module Integer (Limit : LIMIT) :
    TYPE with type t = int =
struct
    type t = int
    let to_edit name getter =
        let size_cstr = Option.map_default (fun x ->
                          let s = float_of_int x |>
                                  log10 |> ceil |>
                                  int_of_float in
                          [ "maxlength", string_of_int s ; "size", string_of_int s ])
                          [] Limit.max in
        [ input (("name", name) ::
                 ("value", input_text_of name getter) ::
                 size_cstr) ]
    let from name getter =
        match getter name with
        | [] | [""] -> missing_field ()
        | s::_ ->
            try let n = int_of_string s in
                let min = Option.default n Limit.min
                and max = Option.default n Limit.max in
                if n < min then (
                    input_error (Printf.sprintf "must not be less than %d" min)
                ) else if n > max then (
                    input_error (Printf.sprintf "must not be greater than %d" max)
                ) else n
            with Failure _ ->
                input_error "not an integer"
end

(** Implementation for an optional input *)
module Optional (Type : TYPE) :
    TYPE with type t = Type.t option =
struct
    type t = Type.t option
    let to_edit = Type.to_edit
    let from name getter =
        try Some (Type.from name getter)
        with InputError s when s = missing_field_str -> None
end


module type LIMIT_FLOAT =
sig
    val min : float option
    val max : float option
end

module NoLimit_float : LIMIT_FLOAT =
struct
    let min = None
    let max = None
end

module Float (Limit : LIMIT_FLOAT) :
    TYPE with type t = float =
struct
    type t = float
    let to_edit name getter =
        [ input [ "name", name ;
                  "value", input_text_of name getter ] ]
    let from name getter =
        match getter name with
        | [] | [""] -> missing_field ()
        | s::_ ->
            try let n = float_of_string s in
                let min = Option.default n Limit.min
                and max = Option.default n Limit.max in
                if n < min then (
                    input_error (Printf.sprintf "must not be less than %f" min)
                ) else if n > max then (
                    input_error (Printf.sprintf "must not be greater than %f" max)
                ) else n
            with Failure _ ->
                input_error "not a real number"
end

module Boolean :
    TYPE with type t = bool =
struct
    type t = bool
    let to_edit name getter =
        let is_true = getter name = ["true"] in
        [ input ([ "type", "radio" ;
                   "name", name ;
                   "value", "true" ] @
                 (if is_true then ["checked","checked"] else [])) ;
          raw "True" ;
          input ([ "type", "radio" ;
                   "name", name ;
                   "value", "false" ] @
                 (if not is_true then ["checked","checked"] else [])) ;
          raw "False" ]
    let from name getter =
        getter name |> List.hd |> bool_of_string
end


module StdString = String
module String (Limit : LIMIT) :
    TYPE with type t = string =
struct
    type t = string
    let to_edit name getter =
        [ input [ "name", name ;
                  "size", string_of_int (min 20 (Limit.max |? 20)) ;
                  "value", input_text_of name getter ] ]
    let from name getter =
        match getter name with
            | [] | [""] -> missing_field ()
            | s::_ ->
                let len = StdString.length s in
                let min = Option.default len Limit.min
                and max = Option.default len Limit.max in
                if len < min then input_error (Printf.sprintf "Input must be at least %d chars long" min)
                else if len > max then input_error (Printf.sprintf "Input must not exceed %d chars" max)
                else s
end

module type ENUM_OPTIONS =
sig
    val name : string
    val options : string array
end

let select_box name ?(any=false) options selected =
    let option_of_value i n =
        tag "option"
            ~attrs:(
                ("value", string_of_int i)::
                (if List.mem i selected then [ "selected", "selected" ] else [])
            )
            [ cdata n ] in
    let opts = Array.mapi option_of_value options |>
               Array.to_list in
    let opts = if any then (
                   tag "option" ~attrs:["value",""] [ cdata "any" ] :: opts
               ) else opts in
    [ tag "select"
          ~attrs:[ "name", name ]
          opts ]

let value_to_idx options v =
    try int_of_string v
    with Failure _ ->
        Array.findi ((=) v) options

module Enum (E : ENUM_OPTIONS) :
    TYPE with type t = int =
struct
    type t = int
    let to_edit name getter =
        let selected = try getter name |>
                           List.map (value_to_idx E.options)
                       with Not_found -> [] in
        select_box name E.options selected
    let from name getter =
        match getter name with
        | [] | [""] -> missing_field ()
        | s::_ ->
            try value_to_idx E.options s
            with Not_found -> input_error "not a valid choice"
end

module OptEnum (E : ENUM_OPTIONS) :
    TYPE with type t = int option =
struct
    include Optional(Enum(E))
    let to_edit name getter =
        let selected = try getter name |>
                           List.map (value_to_idx E.options)
                       with Not_found -> [] in
        select_box name ~any:true E.options selected
end

(** {2 Records} *)

(** We build record fields from a type + user name + uniq name + persistance flag. *)
module type FIELD_SPEC = sig
    val display_name : string
    val uniq_name    : string (* Used in the form name _and_ in the set of persistant values *)
    val persistant   : bool   (* If this uniq_name must be made persistant *)
    module Type : TYPE
end

module FieldOf (F : FIELD_SPEC) :
    (* A field has the same type than a type, although it's not one of course *)
    TYPE with type t = F.Type.t =
struct
    type t = F.Type.t

    let id_of name = name ^"/"^ F.uniq_name

    let to_edit name getter =
        let id = id_of name in
        (* Complete getter with cookies *)
        let getter' =
            if F.persistant then (
                if getter id <> [] then
                    (* If we had a value, don't mess with it *)
                    getter
                else (
                    Dispatch.debug_msg (Printf.sprintf "No value supplied for persistant %s" F.uniq_name) ;
                    (* provide the one from cookies *)
                    (try let s = List.assoc F.uniq_name !Dispatch.current_cookies in
                        let s = decode_cookie s in
                        if s <> "" then (
                            Dispatch.debug_msg (Printf.sprintf "Some value found in cookies for %s" F.uniq_name) ;
                            (fun n -> if n = id then [s] else getter n)
                        ) else (
                            Dispatch.debug_msg (Printf.sprintf "empty value found in cookies for %s" F.uniq_name) ;
                            getter
                        )
                    with Not_found ->
                            Dispatch.debug_msg (Printf.sprintf "No value found in cookies for %s" F.uniq_name) ;
                            getter
                       | Base64.Invalid_char ->
                            Printf.fprintf stderr "Invalid char while decoding cookie value of %s\n" F.uniq_name ;
                            getter)
                )
            ) else (
                (* F is not persistant *)
                getter
            ) in
        [ tr [ th [cdata (F.display_name^":")] ;
               td (F.Type.to_edit id getter') ] ]

    let from name getter =
        let id = id_of name in
        (* complete cookies with getter *)
        if F.persistant then (
            (* Was a value suplied? We must look ourself since F.Type.from could trigger
             * an error if no value was found *)
            match getter id with
            | v::_ when v <> "" ->
                (* We had a value! Save it *)
                Dispatch.debug_msg (Printf.sprintf "Saving value '%s' for persistant %s" v F.uniq_name) ;
                Dispatch.add_cookie F.uniq_name (encode_cookie v) ;
            | _ -> ()
        ) ;
        F.Type.from id getter

    let from name getter =
        try from name getter
        with InputError str -> input_error (F.display_name ^ ": " ^ str)
           | exc -> raise exc

end

(** We can cons types together to form tuples, or fields to form records.
 * Notice that the corresponding internal type is not merely the product of both
 * types, since we want to be able to have some unset fields in an overall
 * record that's valid nonetheless. *)
module ConsOf (T1 : TYPE) (T2 : AGGR_TYPE) :
    AGGR_TYPE with type t = T1.t * T2.t =
struct
    type t = T1.t * T2.t
    let to_edit name getter =
        T1.to_edit name getter @
        T2.to_edit name getter
    let from name getter =
        T1.from name getter,
        T2.from name getter
end

(** Useful as end of ConsOf list *)
module NulType :
    AGGR_TYPE with type t = unit =
struct
    type t = unit
    let to_edit _name _args = []
    let from _name _args = ()
end

module RecordOf (T : AGGR_TYPE) :
    AGGR_TYPE with type t = T.t =
struct
    type t = T.t
    let to_edit name getter =
        [ table (T.to_edit name getter @
                 [ tr [ th ~attrs:["colspan","2"]
                           [ input ["type","submit" ;
                                    "value","submit"] ] ] ]) ]
    let from = T.from
end

