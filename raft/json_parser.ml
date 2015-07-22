open Common
open Yojson.Safe
open State

exception JSON_parsing_failure

let config_from_json (json:json) = 
  match json with
  | `Assoc config -> config
	|> List.assoc "timeout"
	|> function `Int i -> i
	|> fun timeout -> {timeout}
  | _ -> raise JSON_parsing_failure