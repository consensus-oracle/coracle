open Common
open Rpcs
open Io
open State

type eventsig = State.t -> State.t option * rpc output list


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
let receive_vote_request id (pkt:RequestVoteArg.t) (state:State.t) =
  match check_terms pkt.term state, state.mode with
  | Invalid, _ | Same, Leader _| Same, Candidate _ ->
    (None, [PacketDispatch (id,reply state.term false)])
  | Same, Follower f -> (
    match check_vote f with
    | true ->
      (Some {state with mode= Follower {voted_for= Some id}},
        [PacketDispatch (id, reply state.term true)])
    | false ->
      (None, [PacketDispatch (id,reply state.term false)]))
  | Higher,_ ->
    (Some {state with term=pkt.term;
        mode= Follower {voted_for= Some id}},
      [PacketDispatch (id, reply pkt.term true)])

let dispatch_vote_request (state:State.t) id = 
  let open RequestVoteArg in 
  PacketDispatch (id,
  RVA {
  term= state.term;
  last_index= state.last_index;
  last_term=state.last_term; })

let start_follower state = 
  let (min,max) = state.config.election_timeout in
  let timeout = Numbergen.uniform min max in
  (None, [SetTimeout (to_span timeout,Heartbeat)])

let start_election (state:State.t) = 
  let timeout = Numbergen.uniform 0 2000 in
  (Some {state with term=state.term+1; mode=State.candidate},
   CancelTimeout Heartbeat ::
   SetTimeout (timeout,Election) ::
   List.map (dispatch_vote_request state) state.node_ids)

let won (state:State.t) = 
  match state.mode with
  | Candidate cand -> List.length cand.votes_from > 
  (List.length state.node_ids) *2 
  | _ -> false

let receive_vote_reply id (pkt:RequestVoteRes.t) (state:State.t) =
  match check_terms pkt.term state, state.mode with
  | Invalid, _ -> 
    (* packet is from a behind node, ignore it *)
    (None,[])
  | Same, Follower _ ->
    (* invalid vote *)
    (None,[])
  | Same, Candidate cand -> (
    match pkt.votegranted with
    | true -> 
      let state = 
        {state with mode = Candidate 
        {cand with votes_from= add_unique id cand.votes_from}} in
      if won state then Replication.start_leader state else (Some state,[]) 
    | false -> (None,[]))
  | Same, Leader _ ->
    (* ignore votes as no longer needed, I've already won *)
    (None,[])
  | Higher, _ -> 
    (* I am behind and need to update *)
    let (_,events) = start_follower state in
    (Some {state with term=pkt.term; mode=State.follower},
      (cancel_timers state) @ events)