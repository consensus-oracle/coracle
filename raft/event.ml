open Common
open Io
open Rpcs


let eval event state =
	match event with
	| PacketArrival (id,pkt) -> Election_io.receive_pkt id pkt state
	| Startup _ -> Election_io.startup state
  | Timeout timer -> Election_io.receive_timeout timer state

