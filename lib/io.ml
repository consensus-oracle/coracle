(* this module handles the general structures from interfacing
	between pure algorithm backends and the simulation/lwt frontends *)

open Common

type timer = Election | Heartbeat | Leadership

type 'rpc input = 
  | Startup of id 
  | PacketArrival of id * 'rpc
  | Timeout of timer

type 'rpc output = 
  | PacketDispatch of id * 'rpc
  | SetTimeout of span * timer
  | CancelTimeout of timer
