open Common

let rec run ss es =
  match Events.next es with
  | Some ((t,n,e),new_es) ->
    Printf.printf "time:%i id:%i event:%s\n" (int_of_time t) (int_of_id n) (Event.input_to_string e);
    let (new_s,new_e) = Event.eval e (States.get n ss) in
    run (States.set n new_s ss) (Events.add n t new_e new_es)
  | None -> Printf.printf "Done\n%s\n" (Events.string_of_stats es)

let start n loss = 
  let para = Parameters.({n;loss}) in
  run (States.init para) (Events.init para)

open Cmdliner

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
  let cmd_t = Term.(pure start $ n $ loss) in
  match Term.eval (cmd_t, info) with `Ok x -> x |_ -> exit 1

 let () = t