open Common
open Io

module Simulate = 
  functor (C: Protocol.CONSENSUS) -> struct

  let rec run ss es =
  match Events.next es with
  | Some ((t,n,e),new_es) ->
    Printf.printf "time:%i id:%i event:%s\n" (int_of_time t) (int_of_id n) (input_to_string C.msg_to_string e);
    let (new_s,new_e) = C.eval e (States.get n ss) in
    run (States.set n new_s ss) (Events.add n t new_e new_es)
  | None -> Printf.printf "Done\n%s\n" (Events.string_of_stats es)

  let start n loss = 
  let para = Parameters.({n;loss}) in
  let config = C.parse_config "" in
    run (States.init (fun n -> C.init n config) para) (Events.init para)

end