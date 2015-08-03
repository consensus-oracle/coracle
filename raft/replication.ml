open Common
open State
open Io
open Rpcs
open Util

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t

let pull = function Some x -> x

(* form the heartbeat packet *)
let form_heartbeat (state:State.t) id = 
  PacketDispatch (id, AEA AppendEntriesArg.({
  	term = state.term;
  	pre_log_term = state.last_term;
  	pre_log_index = state.last_index;
  	entries = [];
  	commit_index = state.commit_index;
  }))

(* form the heartbeat packet *)
let form_heartbeat_reply (state:State.t) id = 
  PacketDispatch (id, AER AppendEntriesRes.({
  	term = state.term;
  	success = true;
  }))

(* triggered by Leadership timer, dispatch heartbeat packets to all nodes *)
let dispatch_heartbeat (state:State.t) global =
	let global = Global.update_n `AE_SND (List.length state.node_ids) global in
	(None,
		SetTimeout (to_span state.config.heartbeat_interval,Leadership) ::
	  List.map (form_heartbeat state) state.node_ids, global)

(* triggered by receiving an AppendEntries packet, reply to AppendEntries *)
let receive_append_request id (pkt:AppendEntriesArg.t) (state:State.t) global =
	let global = global
		|> Global.update `AE_RCV
		|> Global.update `AE_SND in
	 match check_terms pkt.term state, state.mode with
	 | Invalid, _ -> (None, [form_heartbeat_reply state id], global)
	 | Same, Follower f -> 
	 	(Some {state with mode= Follower {f with leader=Some id}}, 
	 	[reconstruct_heartbeat state; form_heartbeat_reply state id], global)
	 | Same, _ -> (None, [], global)
	 | Higher, _ -> 
	 	let (state,events,global) = step_down pkt.term state global in
	 	(state, (form_heartbeat_reply (pull state) id) :: events, global)

let receive_append_reply id (pkt:AppendEntriesRes.t) (state:State.t) global =
	let global = Global.update `AE_RCV global in
     match check_terms pkt.term state with
	 | Higher -> step_down pkt.term state global
	 | _ -> (None, [], global)

(* start leader, called after winning an election *)
let start_leader (state:State.t) global =
	let global = Global.update `ELE_WON global in
	let state = {state with mode=State.leader state.last_index state.node_ids} in 
	let (_,events,global) = dispatch_heartbeat state global in
  (Some state,
  CancelTimeout Election :: events,
	global)

let constuct_reply id (pkt:ClientArg.t) success (leader_hint: id option) =
	[PacketDispatch (id, CRR ClientRes.({
		seq_num = pkt.seq_num; 
		success = (match success with true -> Some (Success pkt.cmd) | false -> None); 
		leader_hint;
	}))]

let receive_client_request id (pkt:ClientArg.t) (state:State.t) global =
  let global = global
  	|> Global.update `CL_ARG_RCV
  	|> Global.update `CL_RES_SND in
  match state.mode with
  | Leader _ -> 
  	(None, constuct_reply id pkt true None,	global)
  | Follower f -> 
  	(None, constuct_reply id pkt false f.leader,	global)
  | Candidate _ -> 
  	(None, constuct_reply id pkt false None,	global)