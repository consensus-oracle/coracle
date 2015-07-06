open Cmdliner

module R = Simulator.Simulate(Raft)

let t =
  let n =
    Arg.(value & opt int 5 & info ["n";"nodes"] ~docv:"NODES"
      ~doc:"number of nodes to simulate") in
  let loss =
    Arg.(value & opt float 0.0 & info ["l";"loss"] ~docv:"LOSS PROBABILITY"
      ~doc:"probability of packet loss") in
  let doc = "an implementation of the Raft algorithm for simulation" in
  let man = [ `S "BUGS"; `P "bug reports to https://github.com/heidi-ann/ocaml-raft/issues";] in
  let info = Term.info "raft" ~version:"0.2" ~doc ~man in
  let cmd_t = Term.(pure R.start $ n $ loss) in
  match Term.eval (cmd_t, info) with `Ok x -> x |_ -> exit 1

 let () = t