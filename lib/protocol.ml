open Common
open Io

module type CONSENSUS = sig
  
type state
val init: id list -> state

type msg
val input_to_string: msg input -> string
val eval: msg input -> state -> state option * msg output list  

end 