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
  servers: int;
}

type t = {
 term: term;
 mode: mode_state;
 last_index: index;
 last_term: term;
 node_ids: id list;
 config: config;
}

let init id config = {
 term = 1;
 mode = Follower {voted_for=None};
 last_index = 0;
 last_term = 0;
 node_ids = create_nodes config.servers id 1;
 config;
}

let add_node id t = 
  {t with node_ids = add_unique id t.node_ids}

let add_nodes ids t = 
  {t with node_ids = ids@t.node_ids}

let mode_to_json = function
  | Follower f ->
    `Assoc [
      ("mode type", `String "follower");
      ("voted for", match f.voted_for with None -> `String "none" | Some id -> `Int id);
    ]
  | Candidate c ->
    `Assoc [
      ("mode type", `String "candidate");
      ("votes from", `List (List.map (fun id -> `Int id) c.votes_from));
    ]
  | Leader l ->
    `Assoc [
      ("mode type", `String "leader");
    ]

let to_json s =
  `Assoc [
    ("term", `Int s.term);
    ("mode", mode_to_json s.mode);
    ("last log index", `Int s.last_index);
    ("last log term", `Int s.last_term);
    ("peers", `List (List.map (fun i -> `Int i) s.node_ids));
  ]

