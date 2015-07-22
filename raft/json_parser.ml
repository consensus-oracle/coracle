open Common
open Yojson.Safe
open State

exception JSON_parsing_failure

let config_from_json (json:json) = 
  match json with
  | `Assoc config -> 
    let min = List.assoc "election_timeout_min" config |> function `Int i -> i in
    let max = List.assoc "election_timeout_max" config |> function `Int i -> i in
	{election_timeout = (min,max)}
  | _ -> raise JSON_parsing_failure