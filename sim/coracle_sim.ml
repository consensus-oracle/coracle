open Cmdliner
open Common
open Metadata

let pull = function 
  | Some x -> x


let protocol_selector config_file trace output_file no_sanity =
  match Json_handler.get_protocol config_file with
  | `Raft -> 
     let module R = Simulator.Simulate(Raft) in
     R.start config_file trace output_file no_sanity
(*   | `Dummy -> 
    let module D = Simulator.Simulate(Dummy) in 
    D.start config_file trace output_file no_sanity *)


let t =
  (* optional strings without defaults *)
  let config_file =
    Arg.(value & opt string "sample_config.json" & info [config_file.sname;config_file.name] ~docv:config_file.name
      ~doc:config_file.doc) in 
  let output_file =
    Arg.(value & opt (some string) None & info [output_file.sname;output_file.name] ~docv:output_file.name
      ~doc:output_file.doc) in  
  (* flags *)
  let trace =
    Arg.(value & flag & info [trace.sname; trace.name] ~docv:trace.name 
      ~doc:trace.doc) in  
  let no_sanity =
    Arg.(value & flag & info [no_sanity.sname;no_sanity.name] ~docv:no_sanity.name 
      ~doc:no_sanity.doc) in
  let cmd_t = Term.(pure protocol_selector $ config_file $ trace $ output_file $ no_sanity) in
  match Term.eval (cmd_t, Docs.info) with `Ok x -> x |_ -> exit 1

 let () = t