(* Common contains basic type and functions used throughout Raft
	it's designed to be opened in source headers *)

open Sexplib.Conv
open Yojson.Safe

type id = int with sexp
type index = int with sexp
type term = int with sexp

val string_of_id: id -> string
val int_of_id: id -> int
val id_of_int: int -> id

type time = int
type span = int
val to_span:  int -> span
val to_time: int -> time
val compare_time: time -> time -> int
val incr: time -> span -> time
(* returns span in seconds, suitable for sleep *)
val sec_of_span: span -> float
val int_of_time: time -> int

val string_of_option : ('a -> string) -> 'a option -> string

val add_unique: 'a -> 'a list -> 'a list

val split: char -> string -> string list

val string_of_list: ('a -> string) -> 'a list -> string

val create_nodes: int -> int -> int -> int list

exception Not_implemented of string

val bugs_to: string

(* metadata for each simulation parameter *)
type 'a parameter = {
  name: string;
  sname: string;
  doc: string;
  default: 'a option;
  min: 'a option;
  max: 'a option;
}

exception Sanity_check_failure of string
val check_parameter: int -> int parameter -> unit

(* List.assoc for string with more helpful exception *)
val json_assoc: string -> (string * 'a) list -> 'a
val json_assoc_def: string -> (string * 'a) list -> 'a -> 'a
val json_assoc_opt: string -> (string * 'a) list -> 'a option

type cmd = int with sexp
type outcome = Failure | Success of cmd with sexp
type msg = Cmd of cmd | Outcome of outcome 
	| CmdM of id * int * cmd | OutcomeM of id * int * outcome  | Startup

val cmd_to_json: cmd -> json
val outcome_to_json: outcome -> json
val msg_to_json: msg -> json

val pull: 'a option -> 'a

val average: int list -> int
val sum: int list -> int
val min: int list -> int
val max: int list -> int
val map_filter: ('a -> 'b option) -> 'a list -> 'b list
val map_filter_fold: ('a -> 'b -> 'a * 'c option) -> 'a -> 'c list -> 'b list -> 'a * 'c list
val map_fold: ('a -> 'b -> 'a * 'c) -> 'a -> 'c list -> 'b list -> 'a * 'c list

(* generic functions on triples *)
val update_triple: 'a * 'b * 'c -> ('a * 'b * 'c) list -> ('a * 'b * 'c) list
val get_triple_exn: 'a -> ('a * 'b * 'c) list -> 'a * 'b * 'c
val get_triple: 'a -> ('a * 'b * 'c) list -> ('a * 'b * 'c) option
val get_value: 'a -> ('a * 'b) list -> 'b option

(* triple must be in reverse order by 'b (ususally time) *)
val triple_to_doubles: 'b -> ('a * 'b * 'c) list -> ('a * ('b * 'c) list) list