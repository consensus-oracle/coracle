open Common
open Rpcs

type eventsig = State.t -> State.t option * rpc Io.output list

val start_leader: eventsig
val dispatch_heartbeat: eventsig
val receive_append_request: id -> AppendEntriesArg.t -> eventsig
val receive_append_reply: id -> AppendEntriesRes.t -> eventsig