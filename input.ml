open Batteries
open Html

(** {1 Simple datatypes that can be shared with the user} *)

(** The types and utilities you are likely to want to open *)
module Ops = struct

    (** The type values from user, that can be set or erroneous
     * (in which case we keep both an error message and the original input
     * so the user is given a chance to fix it) *)
    type 'a user_value = Error of (string * string) (* when an invalid value was tried (err, value) *)
                       | Value of 'a (* valid value *)

    (** The type of an optional user input *)
    module type TYPE =
    sig
        (** The internal type for these values *)
        type t

        val name : string
        val to_html : t user_value -> html
        val edit : string -> t user_value -> html
        val from_args : string -> (string, string) Hashtbl.t -> t user_value
    end

    (** The type of an aggregate of user inputs, the difference being that
     * there are no erroneous nor unset values *)
    module type AGGR_TYPE =
    sig
        (** The internal type for these values *)
        type t

        val name : string
        val to_html : t -> html
        val edit : string -> t -> html
        val from_args : string -> (string, string) Hashtbl.t -> t
    end

    let html_of_user_value f = function
        | Error (err, inp) -> inp ^ "&nbsp;<i>(error: "^err^")</i>"
        | Value x   -> f x

    let err_msg_of = function
        | Value _ -> []
        | Error (err, _inp) ->
            [ p ~attrs:["class","edit-err"]
                [ cdata err ] ]

    let input_of_user_value f = function
        | Error (_err, inp) -> inp
        | Value x -> f x
end

open Ops

let missing_field = Error ("this field is mandatory", "")

module type LIMIT =
sig
    val min : int option
    val max : int option
end

(** Implementation for an optional, bounded range integer *)
module OptInteger (Limit : LIMIT) :
    TYPE with type t = int option =
struct
    type t = int option
    (* FIXME: add limits in type name *)
    let name = "optional integer"
    let to_html v = [ cdata (html_of_user_value (function None -> "<i>unset</i>"
                                                        | Some i -> string_of_int i) v) ]
    let edit name v =
        let size_cstr = Option.map_default (fun x ->
                          let s = float_of_int x |>
                                  log10 |> ceil |>
                                  int_of_float in
                          [ "maxlength", string_of_int s ; "size", string_of_int s ])
                          [] Limit.max in
        [ input (("name", name) ::
                 ("value", input_of_user_value (function None -> ""
                                                      | Some i -> string_of_int i) v) ::
                 size_cstr) ] @
        err_msg_of v
    let from_args name args =
        match Hashtbl.find_option args name with
        | None | Some "" -> Value None
        | Some s -> try let n = int_of_string s in
                        let min = Option.default n Limit.min
                        and max = Option.default n Limit.max in
                        if n < min then (
                            Error (Printf.sprintf "must not be less than %d" min, s)
                        ) else if n > max then (
                            Error (Printf.sprintf "must not be greater than %d" max, s)
                        ) else Value (Some n)
                    with Failure _ ->
                        Error ("not an integer", s)
end

(* Same, but the integer is now mandatory *)
module Integer (Limit : LIMIT) :
    TYPE with type t = int =
struct
    module O = OptInteger (Limit)
    type t = int
    let name = "integer"
    let make_opt = function
        | Error _ as x -> x
        | Value n -> Value (Some n)
    let to_html v = O.to_html (make_opt v)
    let edit name v = O.edit name (make_opt v)
    let from_args name args =
        match O.from_args name args with
        | Error _ as e -> e
        | Value None -> missing_field
        | Value (Some n) -> Value n
end

module type LIMIT_FLOAT =
sig
    val min : float option
    val max : float option
end

module OptFloat (Limit : LIMIT_FLOAT) :
    TYPE with type t = float option =
struct
    type t = float option
    let name = "optional float"
    let to_html v = [ cdata (html_of_user_value (function None -> "<i>unset</i>"
                                                        | Some i -> string_of_float i) v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value (function None -> ""
                                                       | Some i -> string_of_float i) v ] ] @
        err_msg_of v
    let from_args name args =
        match Hashtbl.find_option args name with
        | None | Some "" -> Value None
        | Some s -> try let n = float_of_string s in
                        let min = Option.default n Limit.min
                        and max = Option.default n Limit.max in
                        if n < min then (
                            Error (Printf.sprintf "must not be less than %f" min, s)
                        ) else if n > max then (
                            Error (Printf.sprintf "must not be greater than %f" max, s)
                        ) else Value (Some n)
                    with Failure _ ->
                        Error ("not a real number", s)
end

(* TODO: Float *)

module OptString (Limit : LIMIT) :
    TYPE with type t = string option =
struct
    type t = string option
    let name = "optional string"
    let to_html v = [ cdata (html_of_user_value (Option.default "<i>unset</i>") v) ]
    let edit name v =
        [ input [ "name", name ;
                  "size", string_of_int (min 20 (Option.default 20 Limit.max)) ;
                  "value", input_of_user_value (Option.default "") v ] ] @
        err_msg_of v
    let from_args name args =
        match Hashtbl.find_option args name with
            | None | Some "" -> Value None
            | Some s ->
                let len = String.length s in
                let min = Option.default len Limit.min
                and max = Option.default len Limit.max in
                if len < min then Error (Printf.sprintf "Input must be at least %d chars long" min, s)
                else if len > max then Error (Printf.sprintf "Input must not exceed %d chars" max, s)
                else Value (Some s)
end

module StdString = String
module String (Limit : LIMIT) :
    TYPE with type t = string =
struct
    module O = OptString (Limit)
    type t = string
    let name = "string"
    let make_opt = function
        | Error _ as x -> x
        | Value n -> Value (Some n)
    let to_html v = O.to_html (make_opt v)
    let edit name v = O.edit name (make_opt v)
    let from_args name args =
        match O.from_args name args with
        | Error _ as e -> e
        | Value None -> missing_field
        | Value (Some n) -> Value n
end

module Password (Limit : LIMIT) :
    TYPE with type t = string =
struct
    include String (Limit)
    let to_html v = [ cdata (match v with Error _ -> ""
                                        | _ -> "*****") ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value identity v ;
                  "type", "password" ] ] @
        err_msg_of v
end

module type ENUM_OPTIONS =
sig
    val name : string val options : string array
end

module Enum (E : ENUM_OPTIONS) :
    TYPE with type t = int =
struct
    type t = int
    let name = "enum of "^E.name
    let to_html v = [ cdata (html_of_user_value (Array.get E.options) v) ]
    let edit name v =
        let option_of_value i =
            tag "option"
                ~attrs:(if v = Value i then [ "selected", "selected" ] else [])
                [ cdata E.options.(i) ] in
        let options = Array.mapi (fun i _n -> option_of_value i)
                                 E.options |>
                      Array.to_list in
        [ tag "select"
              ~attrs:[ "name", name ]
              options ] @
        err_msg_of v
    let from_args name args =
        match Hashtbl.find_option args name with
        | None -> missing_field
        | Some s -> try Value (Array.findi ((=) s) E.options)
                    with Not_found -> Error ("not a valid choice", s)
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
    let name = F.uniq_name
    type t = F.Type.t

    let to_html uv =
        [ tr [ th [cdata (F.display_name^":")] ;
               td (F.Type.to_html uv) ] ]
    let edit form_name uv =
        [ tr [ th [cdata (F.display_name^":")] ;
               td (F.Type.edit (form_name^"."^F.uniq_name) uv) ] ]
    let from_args form_name args =
        let id = form_name^"."^F.uniq_name in
        if not F.persistant then (
            F.Type.from_args id args
        ) else (
            (* Was a value suplied? We must look ourself since F.Type.from_args could trigger
             * an error if no value was found *)
            match Hashtbl.find_option args id with
            | Some v when StdString.length v > 0 ->
                (* We had a value! Maybe we should save it? *)
                (match F.Type.from_args id args with
                | Value v ->
                    Dispatch.debug_msg (Printf.sprintf "Saving value for persistant %s" name) ;
                    (* save a marshaled version under the name 'name' *)
                    Dispatch.add_cookie name (Marshal.to_string v [] |> Base64.str_encode) ;
                    Value v
                | x ->
                    Dispatch.debug_msg (Printf.sprintf "Suplied value unusable for persistant %s" name) ;
                    x)
            | _ ->
                Dispatch.debug_msg (Printf.sprintf "no value supplied for persistant %s" name) ;
                (* no value suplied -> provide the one from cookies *)
                (try let s = List.assoc name !Dispatch.current_cookies in
                    let s = Base64.str_decode s in
                    let v : F.Type.t = Marshal.from_string s 0 in
                    Value v
                with Not_found ->
                        F.Type.from_args id args
                   | Base64.Invalid_char ->
                        Printf.fprintf stderr "Invalid char while decoding cookie value of %s\n" name ;
                        F.Type.from_args id args))
end

(** We can cons types together to form tuples, or fields to form records.
 * Notice that the corresponding internal type is not merely the product of both
 * types, since we want to be able to have some unset fields in an overall
 * record that's valid nonetheless. *)
module ConsOf (T1 : TYPE) (T2 : AGGR_TYPE) :
    AGGR_TYPE with type t = T1.t user_value * T2.t =
struct
    type t = T1.t user_value * T2.t
    let name = T1.name^" * "^T2.name
    let to_html (v1, v2) =
        T1.to_html v1 @
        T2.to_html v2
    let edit name (v1, v2) =
        T1.edit name v1 @
        T2.edit name v2
    let from_args name args =
        T1.from_args name args,
        T2.from_args name args
end

(** Useful as end of ConsOf list *)
module NulType :
    AGGR_TYPE with type t = unit =
struct
    type t = unit
    let name = ""
    let to_html () = []
    let edit _name () = []
    let from_args _name _args = ()
end

module RecordOf (T : AGGR_TYPE) :
    AGGR_TYPE with type t = T.t =
struct
    type t = T.t
    let name = "{ "^T.name^" }"
    let to_html t = [ table (T.to_html t) ]
    let edit name value =
        [ table (T.edit name value @
                 [ tr [ th ~attrs:["colspan","2"]
                           [ input ["type","submit" ;
                                    "value","submit"] ] ] ]) ]
    let from_args = T.from_args
end

