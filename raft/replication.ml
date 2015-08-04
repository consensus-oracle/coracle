open Common
open State
open Io
open Rpcs
open Util

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t

let pull = function Some x -> x

(* form an append entries packet *)
let form_heartbeat (state:State.t) (id,next,_) = 
	let pre_index = next - 1 in
  PacketDispatch (id, AEA AppendEntriesArg.({
  	term = state.term;
  	pre_log_term = pull (get_term_at_index pre_index state.log);
  	pre_log_index = pre_index;
  	entries = get_entries_from_index next state.log;
  	commit_index = state.commit_index;
  }))

(* form the heartbeat packet *)
let form_heartbeat_reply (state:State.t) id (pkt:AppendEntriesArg.t) =
	let success = (pkt.term >= state.term) && 
  		(get_term_at_index pkt.pre_log_index state.log = Some pkt.pre_log_term) in
  PacketDispatch (id, AER AppendEntriesRes.({
  	term = state.term;
  	pre_log_index = pkt.pre_log_index + (if success then (List.length pkt.entries) else 0);
  	success;
  }))

(* triggered by Leadership timer, dispatch heartbeat packets to all nodes *)
let dispatch_heartbeat (state:State.t) global =
	match state.mode with 
	| Leader l -> 
		let global = Global.update_n (`AE `ARG_SND) (List.length state.node_ids) global in
		(None,
			SetTimeout (to_span state.config.heartbeat_interval,Leadership) ::
		  List.map (form_heartbeat state) l.indexes, global)
	| _ -> (* only leaders should dispatch heartbeats *) assert false


let rec generate_sm_requests old_commit new_commit log =
	match old_commit=new_commit with
	| true -> []
	| false -> (LocalDispatch (Cmd (get_entry_at_index (old_commit+1) log))) ::
		generate_sm_requests (old_commit+1) new_commit log

(* triggered by receiving an AppendEntries packet, reply to AppendEntries *)
let receive_append_request id (pkt:AppendEntriesArg.t) (state:State.t) global =
	let global = global
		|> Global.update (`AE `ARG_RCV)
		|> Global.update (`AE `RES_SND) in
	match check_terms pkt.term state with
	| Invalid -> 
		(None, [form_heartbeat_reply state id pkt], global)
	| Same | Higher ->
		let (state,events,global) = step_down pkt.term state global in
		let events = (form_heartbeat_reply (pull state) id pkt) :: events in
		let state = 
			match (pull state).mode with 
			| Follower f -> { (pull state) with mode= Follower {f with leader=Some id}}
			| _ -> assert false in
		let state = 
			{state with log = (add_entries (pkt.pre_log_index,pkt.pre_log_term) pkt.entries state.log)} in
		match pkt.commit_index>state.commit_index with
		| true -> 
				let commit = min [pkt.commit_index; state.last_index] in
				(Some {state with commit_index=commit}, 
					(generate_sm_requests state.commit_index commit state.log) @ events
				,global)
		| false -> (Some state, events, global)


let receive_append_reply id (pkt:AppendEntriesRes.t) (state:State.t) global =
	let global = Global.update (`AE `RES_RCV) global in
   match check_terms pkt.term state, state.mode with
	 | Higher, _-> step_down pkt.term state global
	 | Invalid, _  | Same, Follower _ | Same, Candidate _-> (None,[],global)
	 | Same, Leader l -> 
	 	match pkt.success with
	 	| true -> (* update next and match to index+entries, update commit index *)
	 		let state = update_indexes_success state pkt.pre_log_index id in
	 		let new_commit =  get_commit_index state.commit_index l.indexes in
	 		match state.commit_index = new_commit with
	 		| true -> (* commit index hasn't increased *)
	 		(Some state,[],global)
	 		| false -> (* commit index has increased *)
	 		(Some {state with commit_index=new_commit},
	 		 generate_sm_requests state.commit_index new_commit state.log, global)
	 	| false -> (* decrement next and try again *)
	 		(Some (update_indexes_failed state pkt.pre_log_index id),[],global)
	 		(*TODO: actively try again instead of waiting till next append entries *)

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
  	|> Global.update (`CL `ARG_RCV)
  	|> Global.update (`CL `RES_SND) in
  match state.mode with
  | Leader _ -> 
  	(* TODO: actively dispatch appendentries *)
  	(Some (append_entry state pkt.cmd), constuct_reply id pkt true None,	global)
  | Follower f -> 
  	(None, constuct_reply id pkt false f.leader,	global)
  | Candidate _ -> 
  	(None, constuct_reply id pkt false None,	global)