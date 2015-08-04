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
  history: (cmd * span) list;
  failed: int;
}

let init (para:Parameters.t) n = {
  id = n;
  request = 0;
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
      | Some (cmd,start) -> (Some {s with current=None; history=(cmd,time-start)::s.history},[])
      | None -> (* duplicate success message *) (None,[]) )
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

type state = int list

let init (para:Parameters.t) n = []

let eval time event s = 
  match event with
  | LocalArrival (Cmd c) -> 
    (Some (c::s),
      [ProxyDispatch (Outcome (Success c))])
  | LocalArrival Startup -> (None,[])
  | _ -> assert false

end

let json_of_stats (x: Client.state list) (y:StateMachine.state list) = 
  let times = x
    |> List.map (fun (state:Client.state) -> state.history)
    |> List.flatten
    |> List.map (fun (cmd,time) -> time) in
  let outstanding = x
    |> List.filter (fun (state:Client.state) -> match state.current with None -> false | Some _ -> true)
    |> List.length in
  let failed = x
    |> List.map (fun (state:Client.state) -> state.failed)
    |> sum in
  let commands = x
    |> List.map (fun (state:Client.state) -> state.request)
    |> sum in
  let applied = y
    |> List.map List.length
    |> average in
  match commands with 
    | 0 -> `String "no commands committed"
    | _ -> `Assoc [
      ("commands attempted", `Int commands);
      ("successful", `Int (List.length times));
      ("failed", `Int failed);
      ("outstanding", `Int outstanding);
      ("average time", `Int (average times));
      ("actual times", `List (List.map (fun x -> `Int x) times));
      ("average commands applied per state machine", `Int applied);
      ]