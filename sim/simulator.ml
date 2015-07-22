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

  let start config_file trace output_file no_sanity = 
    let json = Json_handler.json_from_file config_file in
    let para = Json_handler.parameters_from_json json in
    let protocol_json = Json_handler.proto_json_from_json json in
    if no_sanity then () else Parameters.check_sanity para;
    Numbergen.init para.seed;
    let config = C.parse_config protocol_json in
    run (States.init (fun n -> C.init n config) para) (Events.init para) trace output_file

end