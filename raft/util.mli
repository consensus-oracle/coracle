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
val cancel_timers: State.t -> rpc Io.output list
val step_down: int -> eventsig