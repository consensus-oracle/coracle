open Common
open Io
open Yojson.Safe

module Simulate = 
  functor (C: Protocol.CONSENSUS) -> struct

  let trace_buffer = ref []

  let buffer json = trace_buffer := json :: !trace_buffer
  let buffer_many jsons = trace_buffer := jsons @ !trace_buffer

  let json_to_stdout json = 
    pretty_to_channel ~std:true stdout json

  let flush_buffer general protocol = 
    json_to_stdout (
      `Assoc [
        ("trace", `List !trace_buffer);
        ("results", `Assoc [
          ("general", general);
          ("protocol specific", protocol);
        ]);
      ])

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
    List.map (output_event_to_json time id) events


  let state_to_json time id state = 
    `Assoc [
     ("time", `Int time);
     ("id", `Int id);
     ("event", C.state_to_json state); ]

  let rec run ss es trace output_file global =
    let rec eval ss es g =
      let open Events in 
      match Events.next es with
      | Next ((t,n,e),new_es) ->
        if trace then buffer (input_event_to_json t n e) else ();
        let (new_s,new_e,new_g) = C.eval e (States.get n ss) g in 
        if trace then buffer_many (output_events_to_json t n new_e) else ();
        (
        match trace, new_s with
        | true, Some state -> buffer (state_to_json t n state)
        | _ -> ());
        eval (States.set n new_s ss) (Events.add n t new_e new_es) new_g
      | NoNext new_es -> 
        flush_buffer (Events.json_of_stats new_es) (C.global_to_json g) in 
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