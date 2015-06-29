open Common

val input_to_string: Io.input -> string
val output_to_string: Io.output -> string

val eval: Io.input -> State.t -> State.t option * Io.output list
