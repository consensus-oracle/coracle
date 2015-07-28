open Common
open Io
open Yojson.Safe

type state = {
  peers: id list;
  counter: int;
  say_hello_to: int;
}

type config = int
let parse_config (json:json) = 
  match json with 
  | `Assoc config -> config
  |> List.assoc "id"
  |> function `Int i -> i

let init peers id = {peers; counter=0; say_hello_to=id}
let add_peers new_peers s = {s with peers=s.peers@new_peers}
let state_to_json s = `Assoc [("counter", `Int s.counter)]

type msg = Hello | HelloBack
let msg_to_json = function
  | Hello -> `String "hello"
  | HelloBack -> `String "hello back"

let msg_serialize x = to_string (msg_to_json x)

let msg_deserialize = function
  | "hello" -> Hello
  | "hello back" -> HelloBack


type global = unit
let global_to_json () = `Assoc []
let reset_global = ()
let set_time _ _ = ()

let eval event state global =
  let (new_s, new_e) =
    match event with
    | PacketArrival (id,Hello) -> (None, [PacketDispatch (id, HelloBack)])
    | PacketArrival (_,HelloBack) -> (Some {state with counter=state.counter +1}, [])
    | Startup _ -> (None, [PacketDispatch (state.say_hello_to, Hello)]) in
  (new_s, new_e, global)
