open Common
open Yojson.Safe
open State

let maybe_apply f = function
  | None -> None
  | Some x -> Some (f x)

let extract_int (json:json) = 
  match json with `Int i -> i

exception JSON_parsing_failure

let config_from_json servers _ (json:json) = 
  match json with
  | `Assoc config -> 
    let min = json_assoc "election_timeout_min" config |> extract_int in
    let max = json_assoc "election_timeout_max" config |> extract_int in
    let interval = json_assoc "heartbeat_interval" config |> extract_int in
    let client_timer = json_assoc_opt "heartbeat_interval" config |> maybe_apply extract_int in
	{ election_timeout = (min,max);
	 heartbeat_interval = interval;
	 servers;
   client_timer;
	}
  | _ -> raise JSON_parsing_failure