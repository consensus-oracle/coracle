open Common
open State
open Io

(* start leader, called after winning an election *)
let start_leader (state:State.t) =
  (Some {state with mode=State.leader},
  [CancelTimeout Election])