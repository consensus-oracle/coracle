open Common
open Yojson.Safe

type follower = {
  voted_for: id option;
  leader: id option;
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
  leader = None;
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
  client_timer: int option;
}

type entry = index * term * cmd
type log = entry list

type t = {
 term: term;
 mode: mode_state;
 last_index: index;
 last_term: term;
 log: log;
 commit_index: index;
 node_ids: id list;
 config: config;
}

let init id config = {
 term = 1;
 mode = follower;
 last_index = 0;
 last_term = 0;
 node_ids = create_nodes config.servers id 1;
 log = [];
 commit_index = 0;
 config;
}

let refresh t = {
  term=t.term;
  mode= follower;
  last_index = 0;
  last_term = 0;
  log = t.log;
  node_ids=t.node_ids;
  commit_index = 0;
  config=t.config
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
      ("leader", match f.leader with None -> `String "none" | Some id -> `Int id);
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

let entry_to_json (index,term,cmd) = 
  `Assoc [
    ("index",`Int index);
    ("term", `Int term);
    ("cmd", `Int cmd);
    ]

let to_json s =
  `Assoc [
    ("term", `Int s.term);
    ("mode", mode_to_json s.mode);
    ("last log index", `Int s.last_index);
    ("last log term", `Int s.last_term);
    ("commit index", `Int s.commit_index);
    ("peers", `List (List.map (fun i -> `Int i) s.node_ids));
    ("log",`List (List.map entry_to_json s.log));
  ]

