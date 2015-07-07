open Cmdliner
open Common

module R = Io_handlers.UnixInterface(Raft)

let t =
  let id =
    Arg.(required & pos 0 (some int) None & info [] ~docv:"ID"
      ~doc:"set the node ID") in
  let max =
    Arg.(value & opt int 0 & info ["max"] ~docv:"MAX_ID"
      ~doc:"max node ID") in
  let doc = "an implementation of the Raft algorithm for Unix" in
  let man = [ `S "BUGS"; `P "bug reports to https://github.com/heidi-ann/ocaml-raft/issues";] in
  let info = Term.info "raft" ~version:"0.2" ~doc ~man in
  let cmd_t = Term.(pure R.setup $ id $ max) in
  match Term.eval (cmd_t, info) with `Ok x -> x |_ -> exit 1

 let () = Lwt_main.run t