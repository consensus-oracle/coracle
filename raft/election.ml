open Common
open Rpcs
open Io
open State

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t


type term_checker =
  | Invalid 
  | Same
  | Higher

type mode = Follower | Candidate | Leader

let string_of_mode = function
  | Follower -> "Follower"
  | Candidate -> "Candidate"
  | Leader -> "Leader"

(* check term of incoming packet relative to local term *)
let check_terms incoming_term (state:State.t) = 
  if incoming_term > state.term then Higher
  else if incoming_term < state.term then Invalid
  else Same

let check_vote (f:State.follower) = 
  match f.voted_for with
  | None -> true 
  | Some _ -> false


let reply term yes = 
  let open RequestVoteRes in 
  RVR {
  term=term; 
  votegranted=yes;
  }

let cancel_timers (state:State.t) = 
 match state.mode with
  | Follower _ -> [CancelTimeout Heartbeat]
  | Candidate _ -> [CancelTimeout Election]
  | Leader _ -> [CancelTimeout Leadership]

(* process incoming RequestVotes RPC *)
let receive_vote_request id (pkt:RequestVoteArg.t) (state:State.t) (global:Global.t) =
  match check_terms pkt.term state, state.mode with
  | Invalid, _ | Same, Leader _| Same, Candidate _ ->
    (None, [PacketDispatch (id,reply state.term false)], global)
  | Same, Follower f -> (
    match check_vote f with
    | true ->
      (Some {state with mode= Follower {voted_for= Some id}},
        [PacketDispatch (id, reply state.term true)], global)
    | false ->
      (None, [PacketDispatch (id,reply state.term false)], global))
  | Higher,_ ->
    (Some {state with term=pkt.term;
        mode= Follower {voted_for= Some id}},
      [PacketDispatch (id, reply pkt.term true)], global)

let dispatch_vote_request (state:State.t) id = 
  let open RequestVoteArg in 
  PacketDispatch (id,
  RVA {
  term= state.term;
  last_index= state.last_index;
  last_term=state.last_term; })

let start_follower state global = 
  let (min,max) = state.config.election_timeout in
  let timeout = Numbergen.uniform min max in
  (None, [SetTimeout (to_span timeout,Heartbeat)], global)

let start_election (state:State.t) global = 
  let timeout = Numbergen.uniform 0 2000 in
  (Some {state with term=state.term+1; mode=State.candidate},
   CancelTimeout Heartbeat ::
   SetTimeout (timeout,Election) ::
   List.map (dispatch_vote_request state) state.node_ids,
   global)

let won (state:State.t) = 
  match state.mode with
  | Candidate cand -> List.length cand.votes_from > 
  (List.length state.node_ids) *2 
  | _ -> false

let receive_vote_reply id (pkt:RequestVoteRes.t) (state:State.t) (global:Global.t) =
  match check_terms pkt.term state, state.mode with
  | Invalid, _ -> 
    (* packet is from a behind node, ignore it *)
    (None,[], global)
  | Same, Follower _ ->
    (* invalid vote *)
    (None,[], global)
  | Same, Candidate cand -> (
    match pkt.votegranted with
    | true -> 
      let state = 
        {state with mode = Candidate 
        {cand with votes_from= add_unique id cand.votes_from}} in
      if won state then Replication.start_leader state global else (Some state,[], global) 
    | false -> (None,[], global))
  | Same, Leader _ ->
    (* ignore votes as no longer needed, I've already won *)
    (None,[], global)
  | Higher, _ -> 
    (* I am behind and need to update *)
    let (_,events, global) = start_follower state global in
    (Some {state with term=pkt.term; mode=State.follower},
      (cancel_timers state) @ events, global)