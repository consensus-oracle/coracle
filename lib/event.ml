open Common
open Io
open Rpcs


let input_to_string = function
  | Startup id -> "startup "^ (string_of_id id)
  | PacketArrival (id,pkt) -> "from: "^(string_of_id id) ^" payload: "^(rpc_to_string pkt)
  | Timeout _-> "timeout"

let output_to_string = function
  | PacketDispatch (id,pkt) -> "to: "^(string_of_id id) ^" payload: "^(rpc_to_string pkt)
  | SetTimeout (s,timer) -> "set timeout for"^string_of_float (sec_of_span s)
  | CancelTimeout timer -> "cancel timeout"

let eval event state =
	match event with
	| PacketArrival (id,pkt) -> Election_io.receive_pkt id pkt state
	| Startup _ -> Election_io.startup state
  | Timeout timer -> Election_io.receive_timeout timer state

