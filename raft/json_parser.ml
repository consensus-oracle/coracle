open Common
open Yojson.Safe
open State

exception JSON_parsing_failure

let config_from_json (json:json) = 
  match json with
  | `Assoc config -> 
    let min = json_assoc "election_timeout_min" config |> function `Int i -> i in
    let max = json_assoc "election_timeout_max" config |> function `Int i -> i in
    let interval = json_assoc "heartbeat_interval" config |> function `Int i -> i in
	{ election_timeout = (min,max);
	 heartbeat_interval = interval;
	 servers=3; (* TODO *)
	}
  | _ -> raise JSON_parsing_failure