open Batteries

let filter rootdir doc =
    let abspath f = rootdir ^"/"^ f in
    let read_file f =
        let os = IO.output_string () in
        File.with_file_in (abspath f) (fun ic ->
            IO.copy ic os ;
            IO.close_out os) in
    let open Html in
    let rec aux = function
        | Tag ("script", a, []) :: rest
          when AList.(mem a "type" ~value_match:(starts_with "text/"))
            && AList.(mem a "src" ~value_match:(starts_with "http://" %> not)) ->
          let src = AList.find a "src" in
          let content = read_file src in
          Tag ("script",
               AList.filter_out ["src"] a,
               [ Raw content ]) ::
          aux rest
        | Tag ("link", a, []) :: rest
          when AList.(mem a "type" ~value_match:(starts_with "text/"))
            && AList.(mem a "rel" ~value_match:((=) "stylesheet"))
            && AList.(mem a "href" ~value_match:(starts_with "http://" %> not)) ->
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

