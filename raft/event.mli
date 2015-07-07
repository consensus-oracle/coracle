open Common
open Rpcs

val eval: rpc Io.input -> State.t -> State.t option * rpc Io.output list
