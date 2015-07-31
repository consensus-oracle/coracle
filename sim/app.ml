open Common
open Io

module Client = struct

type state = {
  id: int;
  request: cmd;
  workload: Numbergen.distribution option;
  current: (cmd * time) option;
  history: (cmd * span) list;
}

let init (para:Parameters.t) n = {
  id = n;
  request = 0;
  workload = para.workload; 
  current = None;
  history = [];
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
  | LocalArrival (Outcome Failure) -> (None,[])
  | LocalTimeout -> 
    (Some {s with request=s.request+1; current= Some (s.request,time)}, [
      ProxyDispatch (Cmd s.request); 
      LocalSetTimeout (Numbergen.generate (pull s.workload));
    ])
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
  | _ -> assert false

end

let json_of_stats x = `Assoc []