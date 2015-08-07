open Common
open Yojson.Safe
open Sexplib.Conv

type follower = {
  voted_for: id option;
  leader: id option;
}

type candidate = {
  votes_from: id list;
}

type leader = {
  indexes: (id * index * index) list;
}



let rec get_commit_index curr indexes = 
  let nodes = (List.length indexes) +1 in
  indexes
  |> List.map (fun (id,next,matched) -> matched) 
  |> List.filter (fun m -> m>curr)
  |> List.length
  |> fun n -> 
      if (n+1)*2 > nodes 
      then get_commit_index (curr+1) indexes 
      else curr 

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

let leader last_index node_ids = Leader {
  indexes = List.map (fun id -> (id, last_index+1, 0)) node_ids;
  }

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

type command = id * int * cmd with sexp
type entry = index * term * command with sexp

(* sorted reverse order by index *)
type log = (entry list) with sexp

let rec get_term_at_index index log =
  match index with
  | 0 -> (* use dummy term *) Some 0
  | _ ->
     match log with
     | [] -> None
     | (i,t,_)::_ when index=i -> Some t
     | _::xs -> get_term_at_index index xs

(* assume index is valid *)
let rec get_entry_at_index index = function
  | (i,_,e)::_ when index=i -> e
  | _::xs -> get_entry_at_index index xs

(* returns empty is index is too large *)
let rec get_entries_from_index index = function
  | [] -> []
  | (i,t,e)::xs when index=i -> [(i,t,e)]
  | (i,t,e)::xs when index>i -> []
  | x::xs -> x :: (get_entries_from_index index xs)

(* returns a new log where index is the index of the last entry *)
let rec cut_entries index log = 
  match log with
  | (i,_,_)::xs when i=index -> log
  | _::xs -> cut_entries index xs 

type client_cache = (id * int * outcome) list with sexp
 
type t = {
 term: term;
 mode: mode_state;
 last_index: index;
 last_term: term;
 log: log;
 client_cache: client_cache;
 commit_index: index;
 last_applied: index;
 node_ids: id list;
 config: config;
}

let init id config = {
 term = 0;
 mode = follower;
 last_index = 0;
 last_term = 0;
 node_ids = create_nodes config.servers id 1;
 log = [];
 client_cache = [];
 commit_index = 0;
 last_applied = 0;
 config;
}

let refresh t = {
  term=t.term;
  mode= follower;
  last_index = 0;
  last_term = 0;
  log = t.log;
  client_cache = [];
  node_ids=t.node_ids;
  commit_index = 0;
  last_applied = 0;
  config=t.config
}

let append_entry (t:t) cmd = 
  { t with
    log = (t.last_index+1, t.term, cmd) :: t.log;
    last_index= t.last_index+1;
    last_term = t.term; 
  }

let update_indexes_success (t:t) index id = 
  match t.mode with
  | Leader l ->
    { t with mode = Leader { l with 
      indexes = update_triple (id,index+1,index) l.indexes}}
  | _ -> assert false

let update_indexes_failed (t:t) index id  = 
  match t.mode with
  | Leader l ->
    let (_,next,matched) = get_triple_exn id l.indexes in
    match index = next with
    | true -> 
    { t with mode = Leader { l with 
      indexes = update_triple (id,next-1,matched) l.indexes}}
    | false -> t
  | _ -> assert false

 let rec add_entries (prev_index,prev_term) entries (state:t) =
  match prev_index=state.last_index && prev_term=state.last_term with
  | true -> (* append entries now *) (
    match entries with
    | [] -> (true,state)
    | (i,t,_)::_ ->
      (true, { state with 
        log = entries@ state.log;
        last_index = i;
        last_term = t;
        }))
  | false -> (
    match get_term_at_index prev_index state.log with
    | Some term when term=prev_term -> (* remove extra entries then append new entries *) (
      match entries with
      | [] -> 
        (true, { state with
          log = entries @ (cut_entries prev_index state.log);
          })
      | (i,t,_)::_ ->
        (true, { state with
          log = entries @ (cut_entries prev_index state.log);
          last_index = i;
          last_term = t;
          }))
    | Some _ -> (* we don't have a consistent start point => do nothing *)
      (false,state)
    | None -> (* we are missing terms => do nothing *)
      (false,state))

let add_node id t = 
  {t with node_ids = add_unique id t.node_ids}

let add_nodes ids t = 
  {t with node_ids = ids@t.node_ids}

let id_index_to_json (id,nexti,matchi) =
  `Assoc [
    ("id",`Int id);
    ("next index", `Int nexti);
    ("match index", `Int matchi);
  ]

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
      ("node indexes", `List (List.map id_index_to_json l.indexes));
    ]

let entry_to_json (index,term,(id,seq_num,cmd)) = 
  `Assoc [
    ("index",`Int index);
    ("term", `Int term);
    ("client id", `Int id);
    ("seq #", `Int seq_num);
    ("cmd", `Int cmd);
    ]

let session_to_json (id,seq_num,outcome) =
  `Assoc [
    ("client id", `Int id);
    ("seq #", `Int seq_num);
    ("outcome", outcome_to_json outcome);
  ]

let to_json s =
  `Assoc [
    ("type", `String "state update");
    ("data", `Assoc [
      ("term", `Int s.term);
      ("mode", mode_to_json s.mode);
      ("last log index", `Int s.last_index);
      ("last log term", `Int s.last_term);
      ("commit index", `Int s.commit_index);
      ("last applied", `Int s.last_applied);
      ("peers", `List (List.map (fun i -> `Int i) s.node_ids));
      ("log",`List (List.map entry_to_json s.log));
      ("client request cache", `List (List.map session_to_json s.client_cache));
      ]);
  ]

