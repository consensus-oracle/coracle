open Cmdliner
open Common
open Metadata

module R = Simulator.Simulate(Raft)

let pull = function 
  | Some x -> x

let t =
  let nodes =
    Arg.(value & opt int (pull n.default) & info [n.sname;n.name] ~docv:n.name
      ~doc:n.doc) in
  let loss =
    Arg.(value & opt float (pull loss.default) & info [loss.sname;loss.name] ~docv:loss.name
    ~doc:loss.doc) in
  let termination = 
    Arg.(value & opt int (pull term.default) & info [term.sname;term.name] ~docv:term.name
      ~doc:term.doc) in
  let config_file =
    Arg.(value & opt (some string) None & info [config_file.sname;config_file.name] ~docv:config_file.name
      ~doc:config_file.doc) in  
  let trace =
    Arg.(value & flag & info [trace.sname; trace.name] ~docv:trace.name 
      ~doc:trace.doc) in  
  let output_file =
    Arg.(value & opt (some string) None & info [output_file.sname;output_file.name] ~docv:output_file.name
      ~doc:output_file.doc) in  
  let no_sanity =
    Arg.(value & flag & info [no_sanity.sname;no_sanity.name] ~docv:no_sanity.name 
      ~doc:no_sanity.doc) in
  let seed =
    Arg.(value & opt (some int) None & info [seed.sname;seed.name] ~docv:seed.name
      ~doc:seed.doc) in 
  let cmd_t = Term.(pure R.start $ nodes $ loss $ termination $ config_file $ trace $ output_file $ no_sanity $ seed) in
  match Term.eval (cmd_t, Docs.info) with `Ok x -> x |_ -> exit 1

 let () = t