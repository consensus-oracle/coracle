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
  let termination = 
    Arg.(value & opt int 100 & info ["t";"termination"] ~docv:"TERMINATION TIME"
      ~doc:"termination time") in
  let config_file =
    Arg.(value & opt (some string) None & info ["f";"file"] ~docv:"CONFIG FILE"
      ~doc:"name of config file") in  
  let trace =
    Arg.(value & flag & info ["t"; "trace"] ~docv:"TRACE FLAG" 
      ~doc:"enable tracing") in  
  let output_file =
    Arg.(value & opt (some string) None & info ["o";"output"] ~docv:"OUTPUT FILE"
      ~doc:"name of output file, print to stdout if not set") in  
  let cmd_t = Term.(pure R.start $ n $ loss $ termination $ config_file $ trace $ output_file) in
  match Term.eval (cmd_t, Docs.info) with `Ok x -> x |_ -> exit 1

 let () = t