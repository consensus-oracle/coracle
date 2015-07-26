open Common
open Io

type 'msg event = time * id * 'msg input
type 'msg queue = 'msg event list

type data = {
  msgsent: int;
  msgrecv: int;
  msgdrop: int;
  msgflight: int;
}

let inital_data = {
  msgsent = 0;
  msgrecv = 0;
  msgdrop = 0;
  msgflight = 0;
}

type 'msg t = {
  queue: 'msg queue;
  queue_id: id;
  data: data;
  p: Parameters.t;
  }

let receive_msgs n t =
  { t with data = {t.data with msgrecv=t.data.msgrecv+n; msgflight=t.data.msgflight-n}}

let dispatch_msgs n t =
  { t with data = {t.data with msgsent=t.data.msgsent+n; msgflight=t.data.msgflight+n}}

let drop_msgs n t =
  { t with data = {t.data with msgdrop=t.data.msgdrop+n}}


let count f qu = 
  List.length (List.filter f qu)

let rec map_filter f = function
  | [] -> []
  | x::xs -> (
    match f x with 
    | None -> map_filter f xs 
    | Some y -> y :: (map_filter f xs))

let rec start_events m n =
    if m>n then [] 
  else 
    let id = id_of_int m in 
    (to_time 0, id, Startup id) :: (start_events (m+1) n)

let init p = 
  let n = Parameters.(Network.count_servers p.network) in
  {queue = start_events 1 n; 
  queue_id = 0;
  data=inital_data;
  p}



let next t = 
  match t.queue with
 | [] -> None
 | (time,n,e)::xs -> 
    if (time>=t.p.term) then None 
    else
      (match (time,n,e) with
      | (_,_,PacketArrival (_,_)) -> receive_msgs 1 t
      | _ -> t)
    |> fun t_new -> Some ((time,n,e), {t_new with queue=xs})

let rec add_one t ((time,_,_) as y) = 
  match t.queue with
  | ((et,_,_) as x)::xs -> 
    if (compare_time time et) <=0 then {t with queue=y::x::xs}
    else 
    let t_xs = add_one {t with queue=xs} y in
    {t_xs with queue= x :: (t_xs.queue)}
  | [] -> {t with queue=[y]}


let output_to_input t origin time = function
  | PacketDispatch (dest,pkt) -> (
    match Network.find_path origin dest time t.p.network with 
    | None -> (* no path *) None
    | Some latency -> Some (incr time latency, dest, PacketArrival (origin,pkt)) )
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
  let dispatched = count (function PacketDispatch (_,_) -> true | _ -> false) output_events in 
  let sent = count (function (_,_,PacketArrival (_,_)) -> true | _ -> false) input_events in 
  t
  |> dispatch_msgs dispatched
  |> dispatch_msgs (dispatched-sent)
  |> fun t_new -> List.fold_left add_one t_new input_events


open Yojson.Safe

let json_of_stats t =
  `Assoc [
    ("packets dispatched", `Int t.msgsent);
    ("packets received", `Int t.msgrecv);
    ("packets dropped", `Int t.msgdrop);
    ("packets inflight", `Int t.msgflight);
    ]

let output_of_stats t = function
  | None -> to_channel stdout (json_of_stats t.data)
  | Some filename -> to_file filename (json_of_stats t.data)