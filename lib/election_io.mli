(* This interface file details all incoming I/O functions
	for leader election *)

open Common
open Rpcs

type eventsig = State.t -> State.t option * Io.output list

val receive_pkt: id -> rpc -> eventsig
val receive_timeout: Io.timer -> eventsig
val startup: eventsig