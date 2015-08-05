open Common
open Rpcs

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t

val start_leader: eventsig
val dispatch_heartbeat: eventsig
val receive_append_request: id -> AppendEntriesArg.t -> eventsig
val receive_append_reply: id -> AppendEntriesRes.t -> eventsig
val receive_client_request: id -> ClientArg.t -> eventsig
val receive_sm_response: outcome -> eventsig