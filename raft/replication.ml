open Common
open State
open Io
open Rpcs
open Util

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t

let constuct_reply id seq_num outcome (leader_hint: id option) =
	[PacketDispatch (id, CRR ClientRes.({
		seq_num = seq_num; 
		success = outcome; 
		leader_hint;
	}))]


(* form an append entries packet *)
let form_heartbeat (state:State.t) global (id,next,_) = 
	let pre_index = if next>0 then next - 1 else 0 in
	let entries = get_entries_from_index next state.log in
	let global = global
		|> Global.update (`AE `ARG_SND)
		|> Global.update_n (`CMD_DSP) (List.length entries) in
	(global,
  PacketDispatch (id, AEA AppendEntriesArg.({
  	term = state.term;
  	pre_log_term = pull (get_term_at_index pre_index state.log);
  	pre_log_index = pre_index;
  	entries;
  	commit_index = state.commit_index;
  })))

(* form the heartbeat packet *)
let form_heartbeat_reply (state:State.t) id (pkt:AppendEntriesArg.t) success =
  PacketDispatch (id, AER AppendEntriesRes.({
  	term = state.term;
  	pre_log_index = pkt.pre_log_index + (if success then (List.length pkt.entries) else 0);
  	success;
  }))

(* triggered by Leadership timer, dispatch heartbeat packets to all nodes *)
let dispatch_heartbeat reset (state:State.t) global =
	match state.mode with 
	| Leader l -> (
		let (global,ae_msgs) = map_fold (form_heartbeat state) global [] l.indexes in
		match reset with
		| true -> (* reset instead of set timer *)
			(None,
			ResetTimeout (to_span state.config.heartbeat_interval,Leadership) ::
		  ae_msgs, global)
		| false -> (* already triggered by timer *)
			(None,
			SetTimeout (to_span state.config.heartbeat_interval,Leadership) ::
		  ae_msgs, global))
	| _ -> (* only leaders should dispatch heartbeats *) assert false


let rec generate_sm_requests old_commit new_commit log cache =
	match old_commit=new_commit with
	| true -> []
	| false -> 
		let (client_id, seq_num, cmd) = get_entry_at_index (old_commit+1) log in
		match get_triple client_id cache with
		| Some (_,c_seq,_) when c_seq=seq_num -> 
			generate_sm_requests (old_commit+1) new_commit log cache
		| Some _ | None ->
			(LocalDispatch (CmdM (client_id,seq_num,cmd))) ::
			(generate_sm_requests (old_commit+1) new_commit log cache)

let rec generate_sm_requests_leader new_commit (state:State.t) events global =
	match state.commit_index=new_commit with
	| true -> (Some state,events,global)
	| false -> (
		let (id, seq_num, cmd) = get_entry_at_index (state.commit_index+1) state.log in
		let state = {state with commit_index=state.commit_index+1} in
		match get_triple id state.client_cache with
		| Some (_,c_seq,o) when c_seq=seq_num -> (
			match state.mode with 
			| Leader l -> 
				generate_sm_requests_leader new_commit state
					((constuct_reply id seq_num (Some o) None)@events) (Global.update (`CL `RES_SND) global)
			| _ -> (* the leader will tell client *)
				generate_sm_requests_leader new_commit state events global)
		| Some _ | None ->
			generate_sm_requests_leader new_commit state
				((LocalDispatch (CmdM (id,seq_num,cmd))) :: events) global)

(* triggered by receiving an AppendEntries packet, reply to AppendEntries *)
let receive_append_request id (pkt:AppendEntriesArg.t) (state:State.t) global =
	let global = global
		|> Global.update (`AE `ARG_RCV)
		|> Global.update (`AE `RES_SND) in
	match check_terms pkt.term state with
	| Invalid -> 
		(None, [form_heartbeat_reply state id pkt false], global)
	| Same | Higher ->
		let (state,events,global) = step_down pkt.term state global in
		let state = 
			match (pull state).mode with 
			| Follower f -> { (pull state) with mode= Follower {f with leader=Some id}}
			| _ -> assert false in
		let (success,state) = add_entries (pkt.pre_log_index,pkt.pre_log_term) pkt.entries state in
		let events = (form_heartbeat_reply state id pkt success) :: events in
		match pkt.commit_index>state.commit_index with
		| true -> 
				let commit = min [pkt.commit_index; state.last_index] in
				let (state,sm_requests,global) = generate_sm_requests_leader commit state events global in
				(state, sm_requests,global)
		| false -> (Some state, events, global)


let receive_append_reply id (pkt:AppendEntriesRes.t) (state:State.t) global =
	let global = Global.update (`AE `RES_RCV) global in
   match check_terms pkt.term state, state.mode with
	 | Higher, _-> step_down pkt.term state global
	 | Invalid, _  | Same, Follower _ | Same, Candidate _-> (None,[],global)
	 | Same, Leader _ -> 
	 	match pkt.success with
	 	| true -> ((* update next and match to index+entries, update commit index *)
	 		let state = update_indexes_success state pkt.pre_log_index id in
	 		let l = match state.mode with Leader l -> l in
	 		let new_commit =  get_commit_index state.commit_index l.indexes in
	 		match state.commit_index = new_commit with
	 		| true -> (* commit index hasn't increased *)
	 		(Some state,[],global)
	 		| false -> (* commit index has increased *)
	 		(Some {state with commit_index=new_commit},
	 		 generate_sm_requests state.commit_index new_commit state.log state.client_cache, global))
	 	| false -> (* decrement next and try again *)
	 		let state = update_indexes_failed state pkt.pre_log_index id in
	 		match state.config.lazy_update with
	 		| true -> (Some state,[],global)
	 		| false -> 
	 			let l = match state.mode with Leader l -> l in
	 			let (global,pkt) = form_heartbeat state global (get_triple_exn id l.indexes) in
	 			(Some state, [pkt], global)


(* start leader, called after winning an election *)
let start_leader (state:State.t) global =
	let global = Global.update `ELE_WON global in
	let state = {state with mode=State.leader state.last_index state.node_ids} in 
	let (_,events,global) = dispatch_heartbeat false state global in
  (Some state,
  CancelTimeout Election :: events,
	global)

let receive_client_request id (pkt:ClientArg.t) (state:State.t) global =
  let global = global
  	|> Global.update (`CL `ARG_RCV) in
  match state.mode with
  | Leader l -> (
  	let global = global
  		|> Global.update `CMD_RCV in
  	(* TODO: actively dispatch appendentries *)
  	let request = (id,pkt.seq_num,pkt.cmd) in
  	let state = append_entry state request in
  	match state.config.batch_requests with 
  	| true -> (Some state, [], global)
  	| false -> 
  		let (_,events,global) = dispatch_heartbeat true state global in
  		(Some state, events, global))
  | Follower f -> 
  	(None, constuct_reply id pkt.seq_num None f.leader,	Global.update (`CL `RES_SND) global)
  | Candidate _ -> 
  	(None, constuct_reply id pkt.seq_num None None,	Global.update (`CL `RES_SND) global)

let receive_sm_response (id,seq,o) (state:State.t) global =
  let state = {state with
  (*TODO: remove hardcoded *)
   		  client_cache = update_triple (id,seq,o) state.client_cache;} in
   match state.mode with
   | Leader l -> (* only leaders foward respones to clients *) (
   		(Some state,
   		constuct_reply id seq (Some o) None, Global.update (`CL `RES_SND) global))
   | _ -> (* no client is waiting => ignore *) 
   		(Some state,[],global) 

let fail (state:State.t) global =
	(None,[],global)