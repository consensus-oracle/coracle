open Common
open Rpcs

val input_to_string: rpc Io.input -> string
val output_to_string: rpc Io.output -> string

val eval: rpc Io.input -> State.t -> State.t option * rpc Io.output list
