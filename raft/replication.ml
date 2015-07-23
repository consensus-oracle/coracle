open Common
open State
open Io
open Rpcs

type eventsig = State.t -> State.t option * rpc Io.output list


(* start leader, called after winning an election *)
let start_leader (state:State.t) =
  (Some {state with mode=State.leader},
  [CancelTimeout Election; 
  SetTimeout (to_span state.config.heartbeat_interval,Leadership)])

(* form the heartbeat packet *)
let form_heartbeat (state:State.t) id = 
  PacketDispatch (id, AEA AppendEntriesArg.({term = state.term}))

(* form the heartbeat packet *)
let form_heartbeat_reply (state:State.t) id = 
  PacketDispatch (id, AER AppendEntriesRes.({term = state.term}))

(* triggered by Leadership timer, dispatch heartbeat packets to all nodes *)
let dispatch_heartbeat (state:State.t) =
	(None,
		SetTimeout (to_span 100,Leadership) ::
	  List.map (form_heartbeat state) state.node_ids)

(* triggered by receiving an AppendEntries packet, reply to AppendEntries *)
let receive_append_request id pkt (state:State.t) =
	(None, [form_heartbeat_reply state id])

let receive_append_reply id pkt (state:State.t) =
	(None, [])