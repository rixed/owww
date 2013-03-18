open Batteries

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
end

let filter rootdir doc =
    let starts_with a b = String.starts_with b a in
    let abspath f = rootdir ^"/"^ f in
    let read_file f =
        let os = IO.output_string () in
        File.with_file_in (abspath f) (fun ic ->
            IO.copy ic os ;
            IO.close_out os) in
    let open Html in
    let rec aux = function
        | Tag ("script", a, []) :: rest
          when AList.mem a "type" ~value_match:(starts_with "text/")
            && AList.mem a "src" ~value_match:(starts_with "http://" %> not) ->
          let src = AList.find a "src" in
          let content = read_file src in
          Tag ("script",
               AList.filter_out ["src"] a,
               [ Raw content ]) ::
          aux rest
        | Tag ("link", a, []) :: rest
          when AList.mem a "type" ~value_match:(starts_with "text/")
            && AList.mem a "rel" ~value_match:((=) "stylesheet")
            && AList.mem a "href" ~value_match:(starts_with "http://" %> not) ->
          let src = AList.find a "href" in
          let content = read_file src in
          Tag ("style",
               AList.filter_out ["href"; "rel"] a,
               [ Raw content ]) ::
          aux rest
        | Tag (n, a, c) :: rest ->
            Tag (n, a, aux c) :: aux rest
        | Block h :: rest ->
            Block (aux h) :: aux rest
        | x :: rest -> x :: aux rest
        | [] -> [] in
    aux doc

