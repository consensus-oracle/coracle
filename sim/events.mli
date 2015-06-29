(* Events manages the event queue for a DES *)

open Event
open Common

type t
type event = time * id * Io.input


(* [init p] creates an event queue and adds StartUp events for 
	nodes with ID from 0 to n-1 *)
val init: Parameters.t -> t

val next: t -> (event * t) option

val add: id -> time -> Io.output list -> t -> t

val output_to_input: t -> id -> time -> Io.output -> event option

val string_of_stats: t -> string