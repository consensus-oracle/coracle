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

let eval event state = 
  match event with
  | LocalArrival msg -> (None,[])
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