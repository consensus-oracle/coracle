open Common
open Rpcs

type eventsig = State.t -> State.t option * Io.output list

val receive_vote_request: id -> RequestVoteArg.t -> eventsig
val receive_vote_reply: id -> RequestVoteRes.t -> eventsig
val start_election: eventsig
val start_follower: eventsig