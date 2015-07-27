open Common
open Rpcs
open Io

type state = State.t
type config = State.config
let parse_config = Json_parser.config_from_json
let init = State.init
let add_peers = State.add_nodes
let state_to_json = State.to_json

type msg = Rpcs.rpc
let msg_to_json = Rpcs.rpc_to_json
let msg_serialize = Rpcs.rpc_serialize
let msg_deserialize = Rpcs.rpc_deserialize

type global = {
	ae_pkts: int;
	rv_pkts: int
}

let reset_global = {
	ae_pkts = 0;
	rv_pkts = 0;
}

let global_to_json global = 
	`Assoc [
	("append entries packets", `Int global.ae_pkts);
	("request votes packets", `Int global.rv_pkts);
	]

open Io 

let receive_pkt id pkt state =
  match pkt with
  | RVA x -> Election.receive_vote_request id x state
  | RVR x -> Election.receive_vote_reply id x state
  | AEA x -> Replication.receive_append_request id x state
  | AER x -> Replication.receive_append_reply id x state

let receive_timeout timer (state:State.t) = 
  match timer,state.mode with
  | Heartbeat, Follower _ -> Election.start_election state
  | Election, Candidate _ -> Election.start_election state
  | Leadership, Leader _ -> Replication.dispatch_heartbeat state

let eval event state global =
  let (new_s, new_e) = 
	match event with
	| PacketArrival (id,pkt) -> receive_pkt id pkt state
	| Startup _ -> Election.start_follower state
    | Timeout timer -> receive_timeout timer state in
  (new_s, new_e, global)