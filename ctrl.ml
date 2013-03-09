open Batteries
open Html

module Invalid =
struct
    let run _getter =
        let msg = "OMG! The page you called for does not exist!" in
        [ View.err [ p [cdata msg] ] ]
    let url = "omg" (* any non existent will do *)
end

module Info =
struct
    let run _getter =
        let cgi_params = Cgi.parse_args () in
        let tbl = table
                    ((tr [th [cdata "name"] ;
                          th [cdata "value"]]) ::
                    (List.fold_left
                        (fun l (n, v) ->
                            tr [th [cdata (n^":")] ;
                                td [cdata v]] :: l)
                        []
                        cgi_params)) in
        [ View.err [ tbl ] ]
    let url = ""    (* *)
end
