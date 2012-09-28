(* simple example *)

let _ =
    Dispatch.run (function
        | ["info"] -> Ctrl.Info.run
        | _ -> Ctrl.Invalid.run)
