open Common
open Io

module Client = struct

type state = {
  id: int;
  request: int;
  workload: Numbergen.distribution option;
}

let init (para:Parameters.t) n = {
  id = n;
  request = 0;
  workload = para.workload; 
}

let eval event s = 
  match event with
  | LocalArrival msg -> (
    match msg with 
    | Startup -> (
      match s.workload with
      | Some x -> (None, [LocalSetTimeout (Numbergen.generate x)])
      | None -> (None,[]))
    | Cmd _ -> assert false
    | Outcome _ -> (None,[]))
  | LocalTimeout -> 
    (Some {s with request=s.request+1}, [
      ProxyDispatch (Cmd s.request); 
      LocalSetTimeout (Numbergen.generate (pull s.workload));
    ])
  | _ -> assert false

end 


module StateMachine = struct

type state = int list

let init (para:Parameters.t) n = []

let eval event state = 
  match event with
  | LocalArrival msg -> (None,[])
  | _ -> assert false

end