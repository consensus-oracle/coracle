open Common
open Rpcs
open Io 

type eventsig = State.t -> Global.t -> State.t option * rpc Io.output list * Global.t

type term_checker =
  | Invalid 
  | Same
  | Higher

(* check term of incoming packet relative to local term *)
let check_terms incoming_term (state:State.t) = 
  if incoming_term > state.term then Higher
  else if incoming_term < state.term then Invalid
  else Same

let cancel_timers (state:State.t) = 
 match state.mode with
  | Follower _ -> [CancelTimeout Heartbeat]
  | Candidate _ -> [CancelTimeout Election]
  | Leader _ -> [CancelTimeout Leadership]

let construct_heartbeat (state:State.t) =  
  let (min,max) = state.config.election_timeout in
  let timeout = Numbergen.uniform min max in
  SetTimeout (to_span timeout,Heartbeat)

let reconstruct_heartbeat (state:State.t) =  
  let (min,max) = state.config.election_timeout in
  let timeout = Numbergen.uniform min max in
  ResetTimeout (to_span timeout,Heartbeat)

let step_down term (state:State.t) (global:Global.t) =
   (Some {state with term=term; mode=State.follower},
      (construct_heartbeat state)::(cancel_timers state), 
  		global)

let rec get_commit_index curr indexes = 
  let nodes = (List.length indexes) +1 in
  List.map (fun (id,next,matched) -> matched) indexes
  |> List.filter (fun m -> m>curr)
  |> List.length
  |> fun n -> 
      if (n+1)*2 > nodes 
      then get_commit_index (curr+1) indexes 
      else curr 