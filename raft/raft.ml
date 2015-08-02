open Common
open Rpcs
open Io
open Yojson.Safe

type config = State.config
let parse_config = Json_parser.config_from_json

type msg = Rpcs.rpc
let msg_to_json = Rpcs.rpc_to_json
let msg_serialize = Rpcs.rpc_serialize
let msg_deserialize = Rpcs.rpc_deserialize

type global = Global.t
let reset_global = Global.init
let global_to_json = Global.to_json
let set_time = Global.set_time

module type PROXY = sig
    type state
    val init: id -> config -> state
    val state_to_json: state -> json
    val eval: msg input -> state -> global -> state option * msg output list * global
  end

module Server = struct
  open Io 

  type state = State.t
  let init = State.init
  let state_to_json = State.to_json

  let receive_pkt id pkt state =
    match pkt with
    | RVA x -> Election.receive_vote_request id x state
    | RVR x -> Election.receive_vote_reply id x state
    | AEA x -> Replication.receive_append_request id x state
    | AER x -> Replication.receive_append_reply id x state

  let receive_timeout timer (state:State.t) = 
    match timer,state.mode with
    | Heartbeat, Follower _ -> Election.start_election state 
    | Election, Candidate _ -> Election.restart_election state
    | Leadership, Leader _ -> Replication.dispatch_heartbeat state
    | _ -> (* should not happen *) (fun g -> (None,[],g))

  let eval event state global =
  	match event with
  	| PacketArrival (id,pkt) -> receive_pkt id pkt state global
  	| Startup _ -> Election.start_follower state global
    | Recovery -> Election.restart state global
    | Timeout timer -> receive_timeout timer state global
    | LocalArrival _ -> assert false
    | ProxyArrival _ -> assert false 
end

module Client = struct

  type state = Client_proxy.state
  let init = Client_proxy.init
  let state_to_json = Client_proxy.state_to_json

  let eval event state global =
    match event with
    | PacketArrival (id,CRR pkt) -> Client_proxy.receive_client_request_reply id pkt state global
    | Timeout timer -> Client_proxy.receive_timeout timer state global
    | LocalArrival (Cmd cmd) -> Client_proxy.recieve_client_request cmd state global
    |  _ -> (None,[],global) 
end