(* Events manages the event queue for a DES *)
open Common
open Io
open Yojson.Safe

type 'msg event = time * id * 'msg input
type 'msg queue = 'msg event list
type 'msg t 

type 'msg outcome = Next of ('msg event * 'msg t) | NoNext of 'msg t

(* [init p] creates an event queue and adds StartUp events for 
	nodes with ID from 0 to n-1 *)
val init: Parameters.t -> 'msg t

val next: 'msg t -> 'msg outcome

val add: id -> time -> 'msg output list -> 'msg t -> 'msg t

val output_to_input:  id -> time -> 'msg t -> 'msg output -> 'msg t * 'msg event option

val json_of_stats: 'msg t  -> json