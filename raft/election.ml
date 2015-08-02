open Common
open Rpcs
open Io
open State
open Util

let reply term yes = 
  let open RequestVoteRes in 
  RVR {
  term=term; 
  votegranted=yes;
  }

(* process incoming RequestVotes RPC *)
let receive_vote_request id (pkt:RequestVoteArg.t) (state:State.t) (global:Global.t) =
  let global = 
    Global.update `RV_RCV global
    |> Global.update `RV_SND  in
  match check_terms pkt.term state, state.mode with
  | Invalid, _ | Same, Leader _| Same, Candidate _ ->
    (None, [PacketDispatch (id,reply state.term false)], global)
  | Same, Follower f -> (
    match f.voted_for with
    | None ->
      (Some {state with mode= Follower {f with voted_for= Some id}},
        [PacketDispatch (id, reply state.term true)], global)
    | Some _ ->
      (None, [PacketDispatch (id,reply state.term false)], global))
  | Higher,_ ->
    (Some {state with term=pkt.term;
        mode= Follower {voted_for= Some id; leader=None}},
    [PacketDispatch (id, reply pkt.term true);
    reconstruct_heartbeat state], global)

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
  (None, [construct_heartbeat state], global)

let restart state global = 
  let cancel_events = cancel_timers state in
  let state = refresh state in
  let (_,events,global) = start_follower state global in
  (Some state, cancel_events@events,global)

let start_election (state:State.t) global =
  let global = global
    |> Global.update `ELE_START
    |> Global.update_n `RV_SND (List.length state.node_ids) in 
  let timeout = Numbergen.uniform 0 2000 in
  let state = {state with term=state.term+1; mode=State.candidate} in
  (Some state,
   SetTimeout (timeout,Election) ::
   List.map (dispatch_vote_request state) state.node_ids,
   global)

let restart_election state global = 
  start_election state (Global.update `ELE_RESTART global)

let won (state:State.t) = 
  match state.mode with
  | Candidate cand -> (List.length cand.votes_from +1) *2 > 
  (List.length state.node_ids +1)
  | _ -> false

let receive_vote_reply id (pkt:RequestVoteRes.t) (state:State.t) (global:Global.t) =
  let global = Global.update `RV_RCV global in
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
    let global = Global.update `ELE_DOWN global in
    step_down pkt.term state (Global.update `ELE_DOWN global)