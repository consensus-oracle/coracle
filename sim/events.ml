open Common
open Io

type 'msg event = time * id * 'msg input
type 'msg queue = 'msg event list

type 'msg t = {
  queue: 'msg queue;
  queue_id: id;
  msgsent: int;
  msgrecv: int;
  msgdrop: int;
  p: Parameters.t;
  }

let count f qu = 
  List.length (List.filter f qu)

let rec map_filter f = function
  | [] -> []
  | x::xs -> (
    match f x with 
    | None -> map_filter f xs 
    | Some y -> y :: (map_filter f xs))

let rec start_events m n =
    if m=n then [] 
  else 
    let id = id_of_int m in 
    (to_time 0, id, Startup id) :: (start_events (m+1) n)

let init p = 
  let n = Parameters.(p.n) in
  {queue = start_events 0 n; 
  queue_id = 0;
  msgsent=0;
  msgrecv=0;
  msgdrop=0;
  p}



let next t = 
  match t.queue with
 | [] -> None
 | x::xs -> 
    (match x with
    | (_,_,PacketArrival (_,_)) -> {t with msgrecv=t.msgrecv+1}
    | _ -> t)
    |> fun t_new -> Some (x, {t_new with queue=xs})

let rec add_one t ((time,_,_) as y) = 
  match t.queue with
  | ((et,_,_) as x)::xs -> 
    if (compare_time time et) <=0 then {t with queue=y::x::xs}
    else 
    let t_xs = add_one {t with queue=xs} y in
    {t_xs with queue= x :: (t_xs.queue)}
  | [] -> {t with queue=[y]}


let output_to_input t origin time = function
  | PacketDispatch (dest,pkt) -> 
  if (Numbergen.maybe Parameters.(t.p.loss)) then None
  else Some (incr time (to_span 30), dest, PacketArrival (origin,pkt))
  | SetTimeout (n,timer) -> Some (incr time n,origin,Timeout timer)
  | CancelTimeout _ -> 
    (*should have been removed by cancel_timers *)
    None
    


let rec cancel_timers id q = function
  | (CancelTimeout timer)::xs ->
    cancel_timers id 
    (List.filter (function (_,e_id,Timeout e_timer) 
      when e_id=id && e_timer=timer -> false | _ -> true) q) xs
  | _::xs -> cancel_timers id q xs
  | [] -> q

let add id time output_events t =
  let q = cancel_timers id t.queue output_events in
  let t = {t with queue=q} in 
  let input_events = map_filter (output_to_input t id time) output_events in
  let dispatch = count (function PacketDispatch (_,_) -> true | _ -> false) output_events in 
  let send = count (function (_,_,PacketArrival (_,_)) -> true | _ -> false) input_events in 
  {t with msgsent= t.msgsent+dispatch; msgdrop=t.msgdrop+(dispatch-send)}
  |> fun t_new -> List.fold_left add_one t_new input_events



let string_of_stats t =
  Printf.sprintf 
  "packets dispatched: %i\npackets received: %i\npackets dropped: %i\n"
  t.msgsent t.msgrecv t.msgdrop

