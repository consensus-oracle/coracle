open Common
open Io

module Client = struct
(* Client handles the client side application behaviour,
  Either client may have only one outstanding request at a time,
  A command is said to have 'failed', if either the client already has an outstanding request
  or Outcome=Failure is returned by the consensus algorithms client side proxy *)


type state = {
  id: int;
  request: cmd;
  workload: Numbergen.distribution option;
  current: (cmd * time) option;
  history: (cmd * time * span) list;
  failed: int;
}

let init (para:Parameters.t) n = {
  id = n;
  request = 1;
  workload = para.workload; 
  current = None;
  history = [];
  failed=0;
}

let eval time event s = 
  match event with
  | LocalArrival Startup -> (
      match s.workload with
      | Some x -> (None, [LocalSetTimeout (Numbergen.generate x)])
      | None -> (None,[]))
  | LocalArrival (Cmd _) -> assert false
  | LocalArrival (Outcome (Success cmd)) -> (
      match s.current with
      | Some (curr_cmd,start) when cmd=curr_cmd -> (Some {s with current=None; history=(cmd,start,time-start)::s.history},[])
      | Some _ | None -> (* duplicate success message *) (None,[]) )
  | LocalArrival (Outcome Failure) -> (
       match s.current with
      | Some _ -> (Some {s with current=None; failed=s.failed+1},[])
      | None -> (* duplicate failure message *) (None,[]) )
  | LocalTimeout -> 
      match s.current with
      | Some _ ->
        (Some {s with request=s.request+1; failed=s.failed+1}, 
          [LocalSetTimeout (Numbergen.generate (pull s.workload))])
      | None ->
        (Some {s with request=s.request+1; current= Some (s.request,time)}, [
          ProxyDispatch (Cmd s.request); 
          LocalSetTimeout (Numbergen.generate (pull s.workload));])
  | _ -> assert false

end 


module StateMachine = struct

type state = (id * int * time * cmd) list

let init (para:Parameters.t) n = []

let eval time event s = 
  match event with
  | LocalArrival (CmdM (id,seq,c)) -> 
    (Some ((id,seq,time,c)::s),
      [ProxyDispatch (OutcomeM (id,seq,Success c))])
  | LocalArrival Startup -> (None,[])
  | _ -> assert false

end

let json_of_stats (x: Client.state list) (y:StateMachine.state list) = 
  let client_history = x
    |> List.map (fun (state:Client.state) -> 
        (List.map (fun (cmd,time,dur) -> (state.id,cmd,time,dur)) state.history))
    |> List.flatten
    |> List.rev in
  let duration = client_history
    |> List.map (fun (_,_,_,dur) -> dur) in
  let outstanding = x
    |> List.filter (fun (state:Client.state) -> match state.current with None -> false | Some _ -> true)
    |> List.length in
  let failed = x
    |> List.map (fun (state:Client.state) -> state.failed)
    |> sum in
  let commands = x
    |> List.map (fun (state:Client.state) -> state.request)
    |> sum in
  let y_all = y
    |> List.flatten in
  let final_stats = client_history
    |> List.map (fun (id,seq,t,dur) -> 
        let client_commits = 
          List.filter (fun (n_id,n_seq,_,_) -> n_id=id && n_seq=seq) y_all in
        let count = List.length client_commits in
        let first_time = client_commits
          |> List.map (fun (_,_,time,_) -> time)
          |> (function [] -> t | xs -> min xs) in
        (id,seq,t,dur,count,first_time-t)) in
  let applied = y
    |> List.map List.length
    |> average in
  match commands with 
    | 0 -> `String "no commands committed"
    | _ -> `Assoc [
      ("commands attempted", `Int commands);
      ("successful", `Int (List.length client_history));
      ("failed", `Int failed);
      ("outstanding", `Int outstanding);
      ("time to commit (successful only)", 
        `Assoc [
          ("average",`Int (average duration));
          ("min", `Int (min duration));
          ("max", `Int (max duration));
        ]);
      ("actual times", `List (List.map (fun (id,seq,time,dur,cmd,first) -> 
          `Assoc [
          ("client id", `Int id);
          ("seq number", `Int seq);
          ("time",`Int time);
          ("duration",`Int dur);
          ("time at first application", `Int first);
          ("state machine applications",`Int cmd);
        ]) final_stats));
      ("average commands applied per state machine", `Int applied);
      ]