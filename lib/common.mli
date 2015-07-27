(* Common contains basic type and functions used throughout Raft
	it's designed to be opened in source headers *)

open Sexplib.Conv

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