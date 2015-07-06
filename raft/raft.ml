type state = State.t
let init = State.init

type msg = Rpcs.rpc
let eval = Event.eval
let input_to_string = Event.input_to_string
