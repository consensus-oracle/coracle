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

type state = {
  id: id;
  (* ordered, newest command first *)
  hist: (id * int * time * cmd) list;
  }

let init (para:Parameters.t) n = {
  id=n;
  hist=[];
}

let eval time event (s:state) = 
  match event with
  | LocalArrival (CmdM (id,seq,c)) -> 
    let state = {s with hist = (id,seq,time,c)::s.hist} in
    (Some state,
      [ProxyDispatch (OutcomeM (id,seq,Success c))])
  | LocalArrival Startup -> (None,[])
  | _ -> assert false

end

open Json_basic

(* convert state machine history to data points for command commited verse time plot *)
let hist_to_cum_stats hist term_time = 
  hist
  |> List.rev
  |> List.mapi (fun i (_,_,time,_) -> (time, i+1))
  |> fun xs -> (0,0)::xs
  |> fill_points term_time 

let json_of_stats term_time (x: Client.state list) (y:StateMachine.state list) = 
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
    |> List.map (fun (state:Client.state) -> state.request-1)
    |> sum in
  let y_all = y
    |> List.map (fun (state:StateMachine.state) -> state.hist)
    |> List.flatten in
  let final_stats = client_history
    |> List.map (fun (id,seq,t,dur) -> 
        let client_commits = 
          List.filter (fun (n_id,_,_,n_seq) -> n_id=id && n_seq=seq) y_all in
        let count = List.length client_commits in
        let first_time = client_commits
          |> List.map (fun (_,_,time,_) -> time)
          |> (function [] -> t | xs -> min xs) in
        (id,seq,t,dur,count,first_time-t)) in
  let applied = y
    |> List.map (fun (state:StateMachine.state) -> List.length state.hist)
    |> average in
  let cmd_figure = y
    |> List.map (fun (sm:StateMachine.state) -> (sm.id, hist_to_cum_stats sm.hist term_time))
    |> fun data -> figure_in_json
      ~title:"Number of commands committed to each state machine over time"
      ~y_axis:"Number of commands committted"
      ~x_axis:"Time"
      ~legand:"Server ID"
      ~x_start:0 ~x_end:term_time ~y_start:0 ~y_end:(max_y_of_data data) ~lines:(List.length y) (data_in_json data) in
  match commands with 
    | 0 -> `String "no commands committed"
    | _ -> `Assoc ([
      ("commands attempted", `Int commands);
      ("successful", `Int (List.length client_history));
      ("failed", `Int failed);
      ("outstanding", `Int outstanding);
      ("average commands applied per state machine", `Int applied);
        ] @ (
      match final_stats with
      | [] -> (* no successful *) []
      | _ -> (* some successful *) [
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
            ("time to first application", `Int first);
            ("state machine applications",`Int cmd);
          ]) final_stats));
        ("figure1", cmd_figure);
        ]))