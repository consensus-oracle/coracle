open Common
open Io

module type CONSENSUS = sig
  
type state
val init: id list -> state
val add_peers: id list -> state -> state
val state_to_string: state -> string 

type msg
val input_to_string: msg input -> string
val output_to_string: msg output -> string
val msg_serialize: msg -> string
val msg_deserialize: string -> msg
val eval: msg input -> state -> state option * msg output list  

end 