(* this module handles the general structures from interfacing
	between pure algorithm backends and the simulation/lwt frontends *)

open Common

type timer = Election | Heartbeat | Leadership

type 'rpc input = 
  | Startup of id 
  | PacketArrival of id * 'rpc
  | Timeout of timer

type 'rpc output = 
  | PacketDispatch of id * 'rpc
  | SetTimeout of span * timer
  | CancelTimeout of timer

let input_to_string rpc_to_string = function
  | Startup id -> "startup "^ (string_of_id id)
  | PacketArrival (id,pkt) -> "from: "^(string_of_id id) ^" payload: "^(rpc_to_string pkt)
  | Timeout _-> "timeout"

let output_to_string rpc_to_string  = function
  | PacketDispatch (id,pkt) -> "to: "^(string_of_id id) ^" payload: "^(rpc_to_string pkt)
  | SetTimeout (s,timer) -> "set timeout for"^string_of_float (sec_of_span s)
  | CancelTimeout timer -> "cancel timeout"