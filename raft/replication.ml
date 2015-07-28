open Common
open State
open Io
open Rpcs
open Util

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t

let pull = function Some x -> x

(* form the heartbeat packet *)
let form_heartbeat (state:State.t) id = 
  PacketDispatch (id, AEA AppendEntriesArg.({term = state.term}))

(* form the heartbeat packet *)
let form_heartbeat_reply (state:State.t) id = 
  PacketDispatch (id, AER AppendEntriesRes.({term = state.term}))

(* triggered by Leadership timer, dispatch heartbeat packets to all nodes *)
let dispatch_heartbeat (state:State.t) global =
	let global = Global.update_n `AE_SND (List.length state.node_ids) global in
	(None,
		SetTimeout (to_span 100,Leadership) ::
	  List.map (form_heartbeat state) state.node_ids, global)

(* triggered by receiving an AppendEntries packet, reply to AppendEntries *)
let receive_append_request id (pkt:AppendEntriesArg.t) (state:State.t) global =
	let global = global
		|> Global.update `AE_RCV
		|> Global.update `AE_SND in
	 match check_terms pkt.term state, state.mode with
	 | Invalid, _ -> (None, [form_heartbeat_reply state id], global)
	 | Same, Follower f -> 
	 	(None, [construct_heartbeat state; form_heartbeat_reply state id] @ (cancel_timers state), global)
	 | Same, _ -> (None, [], global)
	 | Higher, _ -> 
	 	let (state,events,global) = step_down pkt.term state global in
	 	(state, (form_heartbeat_reply (pull state) id) :: events, global)

let receive_append_reply id pkt (state:State.t) global =
	let global = Global.update `AE_RCV global in
	(None, [], global)

(* start leader, called after winning an election *)
let start_leader (state:State.t) global =
	let global = Global.update `ELE_WON global in
	let state = {state with mode=State.leader} in 
	let (_,events,global) = dispatch_heartbeat state global in
  (Some state,
  CancelTimeout Election :: events,
	global)
