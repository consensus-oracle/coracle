open Common
open Io
open Yojson.Safe

module Simulate = 
  functor (C: Protocol.CONSENSUS) -> struct

  let json_to_stdout json = 
    pretty_to_channel ~std:true stdout json

  let input_event_to_json time id event = 
    `Assoc [
      ("time", `Int time);
      ("id", `Int id);
      ("event", input_to_json C.msg_to_json event); 
    ]

  let output_event_to_json time id event = 
    `Assoc [
      ("time", `Int time);
      ("id", `Int id);
      ("event", output_to_json C.msg_to_json event); 
    ]

  let output_events_to_json time id events = 
    `List (List.map (output_event_to_json time id) events)


  let state_to_json time id state = 
    `Assoc [
     ("time", `Int time);
     ("id", `Int id);
     ("event", C.state_to_json state); ]

  let rec run ss es trace output_file global =
    let rec eval ss es g =
      match Events.next es with
      | Some ((t,n,e),new_es) ->
        if trace then json_to_stdout (input_event_to_json t n e) else ();
        let (new_s,new_e,new_g) = C.eval e (States.get n ss) g in 
        if trace then json_to_stdout (output_events_to_json t n new_e) else ();
        (
        match trace, new_s with
        | true, Some state -> json_to_stdout (state_to_json t n state)
        | _ -> ());
        eval (States.set n new_s ss) (Events.add n t new_e new_es) new_g
      | None -> 
        Events.output_of_stats es output_file;
        json_to_stdout (C.global_to_json global) in 
  eval ss es global

  let start config_file trace output_file no_sanity = 
    let json = Json_handler.json_from_file config_file in
    let para = Json_handler.parameters_from_json json in
    let protocol_json = Json_handler.proto_json_from_json json in
    if no_sanity then () else Parameters.check_sanity para;
    Numbergen.init para.seed;
    let config = C.parse_config protocol_json in
    run (States.init (fun n -> C.init n config) para) (Events.init para) trace output_file C.reset_global

end