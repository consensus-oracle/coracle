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
  | Recovery
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
      ("type",`String "startup node")]
  | PacketArrival (id,pkt) ->
    `Assoc [
      ("type",`String "packet arrival");
      ("data", `Assoc [
        ("from", `Int id);
        ("payload", rpc_to_json pkt);
        ]);
      ]
  | Timeout timer -> 
      `Assoc [
      ("type",`String "timeout trigger");
      ("data", `Assoc [
        ("timeout type", `String (timer_to_string timer));
        ]);
      ]
  | ProxyArrival msg ->
      `Assoc [
      ("type",`String "proxy message arrival");
      ("data", msg_to_json msg);
      ]
  | LocalArrival msg ->       
      `Assoc [
      ("type",`String "local message arrival");
      ("data", msg_to_json msg);
      ]
  | LocalTimeout ->       
      `Assoc [
      ("type",`String "time out for local application");
      ]
  | Recovery -> 
      `Assoc [
      ("type",`String "node restarting after failure");
      ]

let output_to_json rpc_to_json  = function
  | PacketDispatch (id,pkt) ->
    `Assoc [
      ("type",`String "packet dispatch");
      ("data", `Assoc [
        ("to", `Int id);
        ("payload", rpc_to_json pkt);
        ]);
      ]
  | SetTimeout (s,timer) ->
    `Assoc [
      ("type",`String "starting timer");
      ("data", `Assoc [
        ("timeout type", `String (timer_to_string timer));
        ("duration", `Int s);
        ]);
      ]
  | ResetTimeout (s,timer) ->
    `Assoc [
      ("type",`String "restarting timer");
      ("data", `Assoc [
        ("timeout type", `String (timer_to_string timer));
        ("duration", `Int s);
        ]);
      ]
  | CancelTimeout timer ->   
    `Assoc [
      ("type",`String "cancelling timer");
      ("data", `Assoc [
        ("timeout type", `String (timer_to_string timer));
        ]);
      ]
  | ProxyDispatch msg ->
      `Assoc [
      ("type",`String "proxy message dispatched");
      ("data", msg_to_json msg);
      ]
  | LocalDispatch msg ->
      `Assoc [
      ("type",`String "local message dispatched");
      ("data", msg_to_json msg);
      ]
  | LocalSetTimeout span ->
      `Assoc [
      ("type",`String "local timeout");]