open Common
open Io
open Yojson.Safe

module type CONSENSUS = sig
  
type state
type config
type global
val parse_config: json -> config
val init: id list -> config -> state
val add_peers: id list -> state -> state
val state_to_json: state -> json
val reset_global: global
val global_to_json: global -> json

type msg
val msg_serialize: msg -> string
val msg_deserialize: string -> msg
val msg_to_json: msg -> json
val eval: msg input -> state -> global -> state option * msg output list * global

end 