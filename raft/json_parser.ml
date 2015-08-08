open Common
open Yojson.Safe
open State

let maybe_apply f = function
  | None -> None
  | Some x -> Some (f x)

let maybe_bool default = function
  | None -> default
  | Some x -> x

let extract_int (json:json) = 
  match json with `Int i -> i

let extract_bool (json:json) = 
  match json with `Bool i -> i

exception JSON_parsing_failure

let config_from_json servers _ (json:json) = 
  match json with
  | `Assoc config -> 
    let min = json_assoc "election_timeout_min" config |> extract_int in
    let max = json_assoc "election_timeout_max" config |> extract_int in
    let interval = json_assoc "heartbeat_interval" config |> extract_int in
    let client_timer = json_assoc_opt "client_timeout" config |> maybe_apply extract_int in
    let batch_requests = json_assoc_opt "batch_requests" config 
      |> maybe_apply extract_bool |> maybe_bool false in
	{ election_timeout = (min,max);
	 heartbeat_interval = interval;
	 servers;
   client_timer;
   batch_requests
	}
  | _ -> raise JSON_parsing_failure