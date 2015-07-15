(* Events manages the event queue for a DES *)
open Common
open Io

type 'msg event = time * id * 'msg input
type 'msg queue = 'msg event list
type 'msg t 


(* [init p] creates an event queue and adds StartUp events for 
	nodes with ID from 0 to n-1 *)
val init: Parameters.t -> 'msg t

val next: 'msg t -> ('msg event * 'msg t) option

val add: id -> time -> 'msg output list -> 'msg t -> 'msg t

val output_to_input: 'msg t -> id -> time -> 'msg output -> 'msg event option

val string_of_stats: 'msg t -> string

val output_of_stats: 'msg t -> string option -> unit