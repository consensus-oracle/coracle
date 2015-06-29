(* this module handles the general structures from interfacing
	between pure algorithm backends and the simulation/lwt frontends *)

open Common
open Rpcs

type timer = Election | Heartbeat | Leadership

type input = 
  | Startup of id 
  | PacketArrival of id * rpc
  | Timeout of timer

type output = 
  | PacketDispatch of id * rpc
  | SetTimeout of span * timer
  | CancelTimeout of timer
