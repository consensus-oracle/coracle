open Common
open Rpcs

type eventsig = State.t -> State.t option * rpc Io.output list


type mode = Follower | Candidate | Leader

val string_of_mode : mode -> string

val receive_vote_request: id -> RequestVoteArg.t -> eventsig
val receive_vote_reply: id -> RequestVoteRes.t -> eventsig
val start_election: eventsig
val start_follower: eventsig