(* this module handles the general structures from interfacing
	between pure algorithm backends and the simulation/lwt frontends *)

open Common
open Yojson.Safe

type timer = Election | Heartbeat | Leadership | Client of int

let timer_to_string = function
  | Election -> "election"
  | Heartbeat -> "heartbeat"
  | Leadership -> "leadership"
  | Client n -> "client "^(string_of_int n)

type 'rpc input = 
  | Startup of id 
  | PacketArrival of id * 'rpc
  | Timeout of timer
  | LocalArrival of msg
  | LocalTimeout
  | ProxyArrival of msg


type 'rpc output = 
  | PacketDispatch of id * 'rpc
  | SetTimeout of span * timer
  | CancelTimeout of timer
  | ResetTimeout of span * timer
  | LocalDispatch of msg
  | LocalSetTimeout of span
  | ProxyDispatch of msg

let input_to_json rpc_to_json = function
  | Startup id -> 
    `Assoc [
      ("event",`String "startup node")]
  | PacketArrival (id,pkt) ->
    `Assoc [
      ("event",`String "packet arrival");
      ("from", `Int id);
      ("payload", rpc_to_json pkt)]
  | Timeout timer -> 
      `Assoc [
      ("event",`String "timeout trigger");
      ("timeout type", `String (timer_to_string timer))]
  | ProxyArrival msg ->
      `Assoc [
      ("event",`String "local message arrival");
      ("payload", msg_to_json msg);
      ]
  | LocalArrival msg ->       
      `Assoc [
      ("event",`String "local message arrival");
      ("payload", msg_to_json msg);
      ]
  | LocalTimeout ->       
      `Assoc [
      ("event",`String "time out for local application");
      ]

let output_to_json rpc_to_json  = function
  | PacketDispatch (id,pkt) ->
    `Assoc [
      ("event",`String "packet dispatch");
      ("to", `Int id);
      ("payload", rpc_to_json pkt)]
  | SetTimeout (s,timer) ->
    `Assoc [
      ("event",`String "starting timer");
      ("timeout type", `String (timer_to_string timer));
      ("duration", `Int s)]
  | ResetTimeout (s,timer) ->
    `Assoc [
      ("event",`String "restarting timer");
      ("timeout type", `String (timer_to_string timer));
      ("duration", `Int s)]
  | CancelTimeout timer ->   
    `Assoc [
      ("event",`String "cancelling timer");
      ("timeout type", `String (timer_to_string timer));]
  | ProxyDispatch msg ->
      `Assoc [
      ("event",`String "local message dispatched");
      ("payload", msg_to_json msg);]