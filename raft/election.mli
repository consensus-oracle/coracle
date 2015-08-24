open Common
open Rpcs
open Util

val receive_vote_request: id -> RequestVoteArg.t -> eventsig
val receive_vote_reply: id -> RequestVoteRes.t -> eventsig
val start_election: eventsig
val restart_election: eventsig
val start_follower: eventsig
val restart: eventsig