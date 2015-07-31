open Common
open Io

type 'msg event = time * id * 'msg input
type 'msg queue = 'msg event list

type termination = Unknown | OutofTime of time * time | OutofEvents

let term_to_string = function
  | Unknown -> "unknown"
  | OutofTime (next,limit)-> Printf.sprintf "timeout at %i next event is at %i" limit next
  | OutofEvents -> "out of events"

type data = {
  msgsent: int;
  msgrecv: int;
  msgdrop: int;
  msgflight: int;
  reason: termination;
  latency: int list;
}

let inital_data = {
  msgsent = 0;
  msgrecv = 0;
  msgdrop = 0;
  msgflight = 0;
  reason = Unknown;
  latency = [];
}

type 'msg t = {
  queue: 'msg queue;
  queue_id: id;
  data: data;
  p: Parameters.t;
  }

type 'msg outcome = Next of ('msg event * 'msg t) | NoNext of 'msg t

let receive_msgs n t =
  { t with data = {t.data with msgrecv=t.data.msgrecv+n; msgflight=t.data.msgflight-n}}

let dispatch_msgs n t =
  { t with data = {t.data with msgsent=t.data.msgsent+n; msgflight=t.data.msgflight+n}}

let drop_msgs n t =
  { t with data = {t.data with msgdrop=t.data.msgdrop+n; msgflight=t.data.msgflight-n}}

let termination_reason r t =
  { t with data = {t.data with reason=r}}

let add_latency n t = 
  { t with data = {t.data with latency=n::t.data.latency}}

let count f qu = 
  List.length (List.filter f qu)

let rec start_events m n =
  if m>n then [] 
  else 
    (to_time 0, m, Startup m) :: (start_events (m+1) n)

let rec start_clients m n =
  if m>n then [] 
  else 
    (to_time 0, m, LocalArrival Startup) :: (start_events (m+1) n)

let init p = 
  let s = Parameters.(Network.count_servers p.network) in
  let c = Parameters.(Network.count_clients p.network) in
  {queue = (start_events 1 (s+c)) @ (start_clients (s+1) (s+c)); 
  queue_id = 0;
  data=inital_data;
  p}



let next t = 
  match t.queue with
 | [] -> NoNext (termination_reason OutofEvents t)
 | (time,n,e)::xs -> 
    if (time>=t.p.term) then NoNext (termination_reason (OutofTime(time,t.p.term)) t)
    else
      (match (time,n,e) with
      | (_,_,PacketArrival (_,_)) -> receive_msgs 1 t
      | _ -> t)
    |> fun t_new -> Next ((time,n,e), {t_new with queue=xs})

let output_to_input origin time t = function
  | PacketDispatch (dest,pkt) -> (
    let t = dispatch_msgs 1 t in
    match Network.find_path origin dest time t.p.network with 
    | None -> (* no path *) (drop_msgs 1 t, None)
    | Some lat -> (add_latency lat t, Some (incr time lat, dest, PacketArrival (origin,pkt)) ))
  | SetTimeout (n,timer) -> (t, Some (incr time n,origin,Timeout timer))
  | CancelTimeout _ | ResetTimeout _ -> 
    (*should have been removed by cancel_timers *)
    (t, None)
  | LocalDispatch m -> (t, Some (time,origin,LocalArrival m))
  | ProxyDispatch m -> (t, Some (time,origin,ProxyArrival m))
  | LocalSetTimeout n -> (t, Some (incr time n,origin,LocalTimeout))
    


let rec cancel_timers time id q = function
  | (CancelTimeout timer)::xs ->
    cancel_timers time id 
    (List.filter (function (_,e_id,Timeout e_timer) 
      when e_id=id && e_timer=timer -> false | _ -> true) q) xs
  | (ResetTimeout (n,timer)::xs) -> 
    cancel_timers time id 
    ((incr time n, id, Timeout timer) ::
    (List.filter (function (_,e_id,Timeout e_timer) 
      when e_id=id && e_timer=timer -> false | _ -> true) q)) xs
  | _::xs -> cancel_timers time id q xs
  | [] -> q


let check_future time input_events = 
  match List.exists (fun (t,_,_) -> t<time) input_events with
  | true -> assert false
  | false -> ()

let rec check_sorted = function
  | [] -> ()
  | [x] -> ()
  | (tx,_,_)::(ty,i,e)::zs when tx<=ty -> check_sorted ((ty,i,e)::zs)
  | _ -> assert false

let compare_events (t1,_,_) (t2,_,_) = compare t1 t2


let add id time output_events t =
  let q = cancel_timers time id t.queue output_events in
  let t = {t with queue=q} in 
  let (t,input_events) = map_filter_fold (output_to_input id time) t [] output_events in
  check_future time input_events;
  let t = {t with queue=(List.sort compare_events (t.queue@input_events))} in
  check_sorted t.queue; t


open Yojson.Safe

let json_of_stats t =
  `Assoc [
    ("packets dispatched", `Int t.data.msgsent);
    ("packets received", `Int t.data.msgrecv);
    ("packets dropped", `Int t.data.msgdrop);
    ("packets inflight", `Int t.data.msgflight);
    ("termination reason", `String (term_to_string t.data.reason));
    ("average latency", `Int (average t.data.latency));
    ]
