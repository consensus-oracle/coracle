open Common
open Yojson.Safe

type follower = {
  voted_for: id option;
}

type candidate = {
  votes_from: id list;
}

type leader = int

type mode_state =
 | Follower of follower
 | Candidate of candidate
 | Leader of leader


let follower = Follower {
  voted_for = None;
  }

let candidate = Candidate {
  votes_from = [];
  }

let leader = Leader 0 

let string_of_mode_state = function
  | Follower _ -> "Follower"
  | Candidate _ -> "Candidate"
  | Leader _ -> "Leader"

type config = {
  election_timeout: int * int;
  heartbeat_interval: int;
}

type t = {
 term: term;
 mode: mode_state;
 last_index: index;
 last_term: term;
 node_ids: id list;
 config: config;
}

let init ids config = {
 term = 1;
 mode = Follower {voted_for=None};
 last_index = 0;
 last_term = 0;
 node_ids = ids;
 config;
}

let add_node id t = 
  {t with node_ids = add_unique id t.node_ids}

let add_nodes ids t = 
  {t with node_ids = ids@t.node_ids}

let mode_to_string = function
  | Follower f ->
    Printf.sprintf 
      "| Mode: Follower
       | VotedFor: %s"
      (string_of_option string_of_int f.voted_for)
  | Candidate c ->
    Printf.sprintf 
      "| Mode: Candidate
       | Votes From: %s"
      (string_of_list string_of_int c.votes_from)
  | Leader l -> 
    "| Mode: Leader"

let to_string s =
  Printf.sprintf 
	"-- NODE STATE ---------------------------------
	| Term: %i
  | Last Log Index: %i
	| Last Log Term: %i
  | Node ids: %s
  %s
  ----------------------------------------------\n"
  (* general state *)
  s.term 
  s.last_index 
  s.last_term
  (string_of_list string_of_int s.node_ids)
  (mode_to_string s.mode)

