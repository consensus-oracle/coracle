type state = State.t
type config = State.config
let parse_config = State.parse_config
let init = State.init
let add_peers = State.add_nodes
let state_to_string = State.to_string

type msg = Rpcs.rpc
let msg_to_string = Rpcs.rpc_to_string
let msg_serialize = Rpcs.rpc_serialize
let msg_deserialize = Rpcs.rpc_deserialize

open Io 
let eval event state =
	match event with
	| PacketArrival (id,pkt) -> Election_io.receive_pkt id pkt state
	| Startup _ -> Election_io.startup state
  | Timeout timer -> Election_io.receive_timeout timer state