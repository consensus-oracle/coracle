open Common
open Io

module Simulate = 
  functor (C: Protocol.CONSENSUS) -> struct

  let rec run ss es trace output_file =
    let output str = if trace then Printf.printf "%s" str else () in
    let rec eval ss es =
      match Events.next es with
      | Some ((t,n,e),new_es) ->
        output (Printf.sprintf "time:%i id:%i event:%s\n"
          (int_of_time t) (int_of_id n) (input_to_string C.msg_to_string e));
        let (new_s,new_e) = C.eval e (States.get n ss) in
        eval (States.set n new_s ss) (Events.add n t new_e new_es)
      | None -> Events.output_of_stats es output_file in 
  eval ss es

  let start n loss term config trace output_file no_sanity seed = 
    let para = match config with
      | None -> Parameters.({n;term;loss})
      | Some filename -> Parameters.of_json filename in
    if no_sanity then (* no sanity checking () *) () else Parameters.check_sanity para;
    Numbergen.init seed;
    let config = C.parse_config "" in
    run (States.init (fun n -> C.init n config) para) (Events.init para) trace output_file

end