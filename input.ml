open Batteries
open Html

(** {1 Simple datatypes that can be shared with the user} *)

(** The types and utilities your are likely to want to open *)
module Ops = struct

    (** The type values from user, that can be set or erroneous
     * (in which case we keep both an error message and the original input
     * so the user is given a chance to fix it) *)
    type 'a user_value = Error of (string * string) | Value of 'a

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
    val min : int
    val max : int
end

(** Implementation for an optional, bounded range integer *)
module OptInteger (Limit : LIMIT) :
    TYPE with type t = int option =
struct
    type t = int option
    let name = "optional integer"
    let to_html v = [ cdata (html_of_user_value (function None -> "<i>unset</i>"
                                                        | Some i -> string_of_int i) v) ]
    let edit name v =
        let max_size = float_of_int Limit.max |>
                       log10 |> ceil |>
                       int_of_float in
        [ input [ "maxlength", string_of_int max_size ;
                  "size", string_of_int max_size ;
                  "name", name ;
                  "value", input_of_user_value (function None -> ""
                                                       | Some i -> string_of_int i) v ] ] @
        err_msg_of v
    let from_args name args =
        match Hashtbl.find_option args name with
        | None -> Value None
        | Some s -> try let n = int_of_string s in
                        if n < Limit.min then (
                            Error (Printf.sprintf "must not be less than %d" Limit.min, s)
                        ) else if n > Limit.max then (
                            Error (Printf.sprintf "must not be greater than %d" Limit.max, s)
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

module String (Limit : LIMIT) :
    TYPE with type t = string =
struct
    type t = string
    let name = "string"
    let to_html v = [ cdata (html_of_user_value identity v) ]
    let edit name v =
        [ input [ "name", name ;
                  "size", string_of_int (min 20 Limit.max) ;
                  "value", input_of_user_value identity v ] ] @
        err_msg_of v
    let from_args name args =
        let s = match Hashtbl.find_option args name with
                | None -> ""
                | Some s -> s in
        let len = String.length s in
        if len < Limit.min then Error (Printf.sprintf "Input must be at least %d chars long" Limit.min, s)
        else if len > Limit.max then Error (Printf.sprintf "Input must not exceed %d chars" Limit.max, s)
        else Value s
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
 * types, since we want to be able to have some unset fields in an overall
 * record that's always valid. *)
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

