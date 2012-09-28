open Batteries
open Html

(** {1 Simple datatypes that can be shared with the user} *)

(** The types and utilities your are likely to want to open *)
module Ops = struct

    (** The type values from user, that can be set or unset
     * (for some reason explained by error msg in string) *)
    type 'a user_value = Unset | Error of (string * string) | Value of 'a

    (** The type of a shareable datatype *)
    module type TYPE =
    sig
        (** The internal type for these values *)
        type t

        val name : string
        val to_html : t user_value -> html
        val edit : string -> t user_value -> html
        val from_args : string -> (string, string) Hashtbl.t -> t user_value
        exception BadInput of (string * string)
        exception Unset
        val from_args_exc : string -> (string, string) Hashtbl.t -> t
    end

    let html_of_user_value f = function
        | Unset            -> "<i>unset</i>"
        | Error (err, inp) -> inp ^ "&nbsp;<i>(error: "^err^")</i>"
        | Value x          -> f x

    let err_msg_of = function
        | Value _ | Unset -> []
        | Error (err, _inp) ->
            [ p ~attrs:["class","edit-err"]
                [ cdata err ] ]

    let input_of_user_value f = function
        | Unset -> ""
        | Error (_err, inp) -> inp
        | Value x -> f x
end

open Ops

(** Implementation for a bounded range integer *)
module Integer (Limit : sig val min : int val max : int end) :
    TYPE with type t = int =
struct
    type t = int
    let name = "integer"
    let to_html v = [ cdata (html_of_user_value string_of_int v) ]
    let edit name v =
        let max_size = float_of_int Limit.max |>
                       log10 |> ceil |>
                       int_of_float in
        [ input [ "maxlength", string_of_int max_size ;
                  "size", string_of_int max_size ;
                  "name", name ;
                  "value", input_of_user_value string_of_int v ] ] @
        err_msg_of v
    let from_args name args =
        match Hashtbl.find_option args name with
        | None -> Unset 
        | Some s -> try let n = int_of_string s in
                        if n < Limit.min then (
                            Error (Printf.sprintf "must not be less than %d" Limit.min, s)
                        ) else if n > Limit.max then (
                            Error (Printf.sprintf "must not be greater than %d" Limit.max, s)
                        ) else Value n
                    with Failure _ ->
                        Error ("not an integer", s)
end

module String :
    TYPE with type t = string =
struct
    type t = string
    let name = "string"
    let to_html v = [ cdata (html_of_user_value identity v) ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value identity v ] ] @
        err_msg_of v
    let from_args name args =
        match Hashtbl.find_option args name with
        | None -> Unset
        | Some s -> Value s
end

module Password :
    TYPE with type t = string =
struct
    include String
    let to_html v = [ cdata (match v with Unset | Error _ -> ""
                                        | Value _ -> "*****") ]
    let edit name v =
        [ input [ "name", name ;
                  "value", input_of_user_value identity v ;
                  "type", "password" ] ] @
        err_msg_of v
end

module Enum (E : sig val name : string val options : string array end) :
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
        | None -> Unset
        | Some s -> try Value (Array.findi ((=) s) E.options)
                    with Not_found -> Error ("not a valid choice", s)
end

(** {2 Records} *)

(** Build a field from a type + field name. *)
module FieldOf (F : sig module Type : TYPE val name : string end) :
    TYPE with type t = F.Type.t =
struct
    include F.Type
    let name = F.name ^ ":" ^ F.Type.name
    let to_html v =
        [ tr [ th [cdata (F.name^":")] ;
               td (F.Type.to_html v) ] ]
    let edit name v =
        [ tr [ th [cdata (F.name^":")] ;
               td (F.Type.edit (name^"/"^F.name) v) ] ]
    let from_args name =
        F.Type.from_args (name^"/"^F.name)
end

(** We can cons types together to form tuples, or fields to form records.
 * Notice that the corresponding internal type is not merely the product of both
 * types, since we want to be able to have some unset fields in an otherwise
 * valid record (ie. record is allowed to be half initialized and still be set!) *)
module ConsOf (T1 : TYPE) (T2 : TYPE) :
    TYPE with type t = T1.t user_value * T2.t user_value =
struct
    type t = T1.t user_value * T2.t user_value
    let name = T1.name^" * "^T2.name
    let values_of_value = function
        (* notice: errors are reported on the first field only *)
        | Unset -> Unset, Unset
        | Error x -> Error x, Unset
        | Value (u1, u2) -> u1, u2
    let value_of_values = function
        (* and the other way round *)
        | Unset, Unset -> Unset
        | Error x, Unset -> Error x
        | u1, u2 -> Value (u1, u2)
    let to_html value =
        let v1, v2 = values_of_value value in
        T1.to_html v1 @
        T2.to_html v2
    let edit name value =
        let v1, v2 = values_of_value value in
        T1.edit name v1 @
        T2.edit name v2
    let from_args name args =
        let v1 = T1.from_args name args
        and v2 = T2.from_args name args in
        value_of_values (v1, v2)
end

module RecordOf (T : TYPE) :
    TYPE with type t = T.t =
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

