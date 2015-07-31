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
    | Timeout timer -> receive_timeout timer state global
    | LocalArrival _ -> assert false
    | ProxyArrival _ -> assert false 
end

module Client = struct

  type state = {
    id: id;
    last_leader: id option;
    servers: int;
    outstanding: (int * cmd) option;
    seq_num: int;
    timeout: int option;
  }

  let try_next state = 
    match state.last_leader with
    | Some id -> id
    | None -> Numbergen.uniform 1 state.servers

  let init id (config:config) = {
    id=id;
    last_leader=None;
    servers = config.servers;
    outstanding =None;
    seq_num = 1;
    timeout = config.client_timer;
  }

  let state_to_json state : json = 
    `Assoc [
      ("last leader", match state.last_leader with None -> `String "none" | Some id -> `Int id);
    ]

  let eval event state global = 
    match event with
    | Timeout (Client n) -> 
      let (seq_num,cmd) = pull state.outstanding in
      let pkt = CRA ClientArg.({seq_num;cmd}) in
      let state = {state with last_leader=None} in
      (Some state,
        [PacketDispatch (try_next state,pkt); 
        SetTimeout (pull state.timeout,Client state.seq_num);
        ],global)
    | PacketArrival (id,CRR pkt) -> (
      match state.outstanding with
      | None -> (* ignore it *) (None,[],global)
      | Some (n,_) when n<>pkt.seq_num -> (* ignore it *) (None,[],global)
      | Some (seq_num,cmd) -> (
        match pkt.success with 
        | Some result -> 
          (Some {state with outstanding=None},
            [LocalDispatch (Outcome result); CancelTimeout (Client seq_num)],global)
        | None -> 
          let pkt = CRA ClientArg.({seq_num;cmd}) in
          let state = {state with last_leader=None} in
          (Some state,[PacketDispatch (try_next state,pkt); ResetTimeout (pull state.timeout,Client seq_num)],global)))
    | ProxyArrival (Cmd client_cmd) -> 
      let pkt = CRA ClientArg.({seq_num = state.seq_num; cmd = client_cmd}) in
      (Some {state with outstanding = Some (state.seq_num, client_cmd); seq_num=state.seq_num+1},
       [PacketDispatch (try_next state,pkt);
        SetTimeout (pull state.timeout,Client state.seq_num)],global)
    | _ -> (None,[],global)
end 