open Common
open Rpcs
open Io

type eventsig = State.t -> State.t option * rpc output list


let receive_pkt id pkt state =
  match pkt with
  | RVA x -> Election.receive_vote_request id x state
  | RVR x -> Election.receive_vote_reply id x state
  | AEA x -> Replication.receive_append_request id x state
  | AER x -> Replication.receive_append_reply id x state

let receive_timeout timer (state:State.t) = 
  match timer,state.mode with
  | Heartbeat, Follower _ -> Election.start_election state
  | Election, Candidate _ -> Election.start_election state
  | Leadership, Leader _ -> Replication.dispatch_heartbeat state

let startup = Election.start_follower 