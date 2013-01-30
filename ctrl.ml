open Batteries
open Html

module Invalid =
struct
    let run _args =
        let msg = "OMG! The page you called for does not exist!" in
        [ View.err [ p [cdata msg] ] ]
    let url = "omg" (* any non existent will do *)
end

module Info =
struct
    let run args =
        let tbl = table
                    ((tr [td [cdata "name"] ;
                         td [cdata "value"]]) ::
                    (Hashtbl.fold (fun n v l ->
                                    tr [th [cdata (n^":")] ;
                                        td [cdata v]] :: l)
                                  args [])) in
        [ View.err [ tbl ] ]
    let url = ""    (* *)
end
