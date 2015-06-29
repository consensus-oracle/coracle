open Common
open Rpcs
open Io

type eventsig = State.t -> State.t option * Io.output list


type term_checker =
  | Invalid 
  | Same
  | Higher

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
      [PacketDispatch (id, reply state.term true)])

let dispatch_vote_request (state:State.t) id = 
  let open RequestVoteArg in 
  PacketDispatch (id,
  RVA {
  term= state.term;
  last_index= state.last_index;
  last_term=state.last_term; })

let start_follower state = 
  let timeout = Numbergen.uniform 0 2000 in
  (None, [SetTimeout (to_span timeout,Heartbeat)])

let start_election (state:State.t) = 
  let timeout = Numbergen.uniform 0 2000 in
  (Some {state with term=state.term+1; mode=State.candidate},
   CancelTimeout Heartbeat ::
   SetTimeout (timeout,Election) ::
   List.map (dispatch_vote_request state) state.node_ids)

let start_leader (state:State.t) =
  (Some {state with mode=State.leader},
  [CancelTimeout Election])

let won (state:State.t) = 
  match state.mode with
  | Candidate cand -> List.length cand.votes_from > 
  (List.length state.node_ids) *2 
  | _ -> false

let receive_vote_reply id (pkt:RequestVoteRes.t) (state:State.t) =
  match check_terms pkt.term state, state.mode with
  | Invalid, _ -> (None,[])
  | Same, Candidate cand | Higher, Candidate cand -> (
    match pkt.votegranted with
    | true -> 
      let state = 
        {state with mode = Candidate 
        {cand with votes_from= add_unique id cand.votes_from}} in
      if won state then start_leader state else (Some state,[]) 
    | false -> (None,[]))
  | Same, Leader _ | Higher, Leader _ ->
    (* ignore votes as no longer needed *)
    (None,[])
  | Same, Follower _ | Higher, Follower _ ->
    (* invalid vote *)
    assert false