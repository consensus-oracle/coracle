(* Rpcs's holds the record which represents Raft's packets *)
open Common

module AppendEntriesArg  = struct
  open Sexplib.Conv

  type t = { 
    term: term;} with sexp

  let to_string t = 
    Printf.sprintf 
    "term: %i\n"
    t.term

  let serialize t =
    sexp_of_t t |> Sexplib.Sexp.to_string

  let deserialize str = 
    Sexplib.Sexp.of_string str |> t_of_sexp

end

module AppendEntriesRes  = struct
  open Sexplib.Conv

  type t = { 
    term: term} with sexp

  let to_string t = 
    Printf.sprintf 
    "term: %i\n"
    t.term

  let serialize t =
    sexp_of_t t |> Sexplib.Sexp.to_string

  let deserialize str = 
    Sexplib.Sexp.of_string str |> t_of_sexp

end

module RequestVoteArg = struct
  open Sexplib.Conv

  type t = { 
    term: term;
    last_index: index;
    last_term: term;} with sexp

  let to_string t = 
    Printf.sprintf 
    "term: %i\nlast index: %i\nlast term: %i\n"
    t.term t.last_index t.last_term

  let serialize t =
    sexp_of_t t |> Sexplib.Sexp.to_string

  let deserialize str = 
    Sexplib.Sexp.of_string str |> t_of_sexp

end

module RequestVoteRes = struct
  open Sexplib.Conv

  type t = {
    term: term;
    votegranted: bool;} with sexp

  let to_string t = 
    Printf.sprintf 
    "term: %i\nlast index: %b\n"
    t.term t.votegranted

  let serialize t =
    sexp_of_t t |> Sexplib.Sexp.to_string

  let deserialize str = 
    Sexplib.Sexp.of_string str |> t_of_sexp

end

type rpc = 
  | RVA of RequestVoteArg.t
  | RVR of RequestVoteRes.t
  | AEA of AppendEntriesArg.t
  | AER of AppendEntriesRes.t with sexp

let rpc_to_string = function
  | RVA x-> RequestVoteArg.to_string x
  | RVR x-> RequestVoteRes.to_string x
  | AEA x-> AppendEntriesArg.to_string x
  | AER x-> AppendEntriesRes.to_string x

let rpc_serialize t =
    sexp_of_rpc t |> Sexplib.Sexp.to_string

let rpc_deserialize str = 
    Sexplib.Sexp.of_string str |> rpc_of_sexp