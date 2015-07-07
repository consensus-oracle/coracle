type state = State.t
let init = State.init
let add_peers = State.add_nodes
let state_to_string = State.to_string

type msg = Rpcs.rpc
let eval = Event.eval
let msg_to_string = Rpcs.rpc_to_string
let msg_serialize = Rpcs.rpc_serialize
let msg_deserialize = Rpcs.rpc_deserialize