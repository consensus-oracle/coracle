open Common
open Rpcs

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t

type term_checker =
  | Invalid 
  | Same
  | Higher

(* check term of incoming packet relative to local term *)
val check_terms: int -> State.t -> term_checker
val construct_heartbeat: State.t -> rpc Io.output
val reconstruct_heartbeat: State.t -> rpc Io.output
val cancel_timers: State.t -> rpc Io.output list
val step_down: int -> eventsig

(* given indexes and a hint index, get_commit_index returns 
	the highest index which can be commited upto *)
val get_commit_index: index -> (id * index * index) list -> index