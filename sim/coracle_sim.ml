open Cmdliner
open Common

module R = Simulator.Simulate(Raft)

let t =
  let n =
    Arg.(value & opt int 5 & info ["n";"nodes"] ~docv:"NODES"
      ~doc:"number of nodes to simulate") in
  let loss =
    Arg.(value & opt float 0.0 & info ["l";"loss"] ~docv:"LOSS PROBABILITY"
      ~doc:"probability of packet loss") in
  let config_file =
    Arg.(value & opt (some string) None & info ["f";"file"] ~docv:"CONFIG FILE"
      ~doc:"name of config file") in    
  let cmd_t = Term.(pure R.start $ n $ loss $ config_file) in
  match Term.eval (cmd_t, Docs.info) with `Ok x -> x |_ -> exit 1

 let () = t