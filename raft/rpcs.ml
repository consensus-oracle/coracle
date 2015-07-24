(* Rpcs's holds the record which represents Raft's packets *)
open Common
open Yojson.Safe

module AppendEntriesArg  = struct
  open Sexplib.Conv

  type t = { 
    term: term;} with sexp

  let to_json t = 
    `Assoc [
      ("packet type", `String "append entries request");
      ("term", `Int t.term); ]

  let serialize t =
    sexp_of_t t |> Sexplib.Sexp.to_string

  let deserialize str = 
    Sexplib.Sexp.of_string str |> t_of_sexp

end

module AppendEntriesRes  = struct
  open Sexplib.Conv

  type t = { 
    term: term} with sexp

  let to_json t = 
    `Assoc [
      ("packet type", `String "append entries reply");
      ("term", `Int t.term); ]

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

  let to_json t = 
    `Assoc [
      ("packet type", `String "request vote request");
      ("term", `Int t.term);
      ("last index", `Int t.last_index);
      ("last term", `Int t.last_term); ]

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

  let to_json t = 
    `Assoc [
      ("packet type", `String "request vote reply");
      ("term", `Int t.term);
      ("vote granted", `Bool t.votegranted);]

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

let rpc_to_json = function
  | RVA x-> RequestVoteArg.to_json x
  | RVR x-> RequestVoteRes.to_json x
  | AEA x-> AppendEntriesArg.to_json x
  | AER x-> AppendEntriesRes.to_json x

let rpc_serialize t =
    sexp_of_rpc t |> Sexplib.Sexp.to_string

let rpc_deserialize str = 
    Sexplib.Sexp.of_string str |> rpc_of_sexp