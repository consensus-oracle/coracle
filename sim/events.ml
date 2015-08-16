open Common
open Io

type 'msg event = time * id * 'msg input
type 'msg queue = 'msg event list

type termination = Unknown | OutofTime of time * time | OutofEvents

let term_to_string = function
  | Unknown -> "unknown"
  | OutofTime (next,limit)-> Printf.sprintf "timeout at %i next event is at %i" limit next
  | OutofEvents -> "out of events"

let compare_events (t1,_,_) (t2,_,_) = compare t1 t2


type data = {
  msgsent: int;
  msgrecv: int;
  msgdrop_nopath: int;
  msgdrop_nodst: int;
  msgflight: int;
  reason: termination;
  latency: int list;
  servers: int;
  clients: int;
  startup: int;
  recover: int;
  failures: int;
}

let inital_data = {
  msgsent = 0;
  msgrecv = 0;
  msgdrop_nopath = 0;
  msgdrop_nodst = 0;
  msgflight = 0;
  reason = Unknown;
  latency = [];
  servers=0;
  clients=0;
  startup=0;
  recover=0;
  failures=0;
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

let drop_msgs_nopath n t =
  { t with data = {t.data with msgdrop_nopath=t.data.msgdrop_nopath+n; msgflight=t.data.msgflight-n}}

let drop_msgs_nodst n t =
  { t with data = {t.data with msgdrop_nodst=t.data.msgdrop_nodst+n; msgflight=t.data.msgflight-n}}

let termination_reason r t =
  { t with data = {t.data with reason=r}}

let add_latency n t = 
  { t with data = {t.data with latency=n::t.data.latency}}

let count f qu = 
  List.length (List.filter f qu)

let rec start_events m n =
  if m>n then [] 
  else 
    (to_time 0, m, Startup m) :: (to_time 0, m, LocalArrival Startup) :: (start_events (m+1) n)

let rec start_clients m n =
  if m>n then [] 
  else 
    (to_time 0, m, Startup m) :: (to_time 0, m, LocalArrival Startup) :: (start_events (m+1) n)

let init p = 
  let s = Parameters.(Network.count_servers p.network) in
  let c = Parameters.(Network.count_clients p.network) in
  let recovery = Network.find_recovery p.network
    |> List.map (fun (id,time) -> (time,id, Recovery)) in
  let fail = Network.find_failure p.network
    |> List.map (fun (id,time) -> (time,id, Fail)) in
  let queue = (start_events 1 (s+c)) @ (start_clients (s+1) (s+c)) @ recovery @ fail in
  {queue = List.sort compare_events queue; 
  queue_id = 0;
  data = {inital_data with 
    servers=s; 
    clients=c; 
    startup=c+s; 
    recover= List.length recovery; 
    failures = List.length fail};
  p}



let rec next t = 
  match t.queue with
 | [] -> NoNext (termination_reason OutofEvents t)
 | (time,n,e)::xs -> 
    if (time>=t.p.term) then NoNext (termination_reason (OutofTime(time,t.p.term)) t)
    else
      match Network.find_node n time t.p.network with
      | true -> 
        (match (time,n,e) with
        | (_,_,PacketArrival (_,_)) -> receive_msgs 1 t
        | _ -> t)
        |> fun t_new -> Next ((time,n,e), {t_new with queue=xs})
      | false -> 
        (match (time,n,e) with
        | (_,_,PacketArrival (_,_)) -> drop_msgs_nodst 1 t
        | _ -> t)
        |> fun t_new -> next {t_new with queue=xs}

let output_to_input origin time t = function
  | PacketDispatch (dest,pkt) -> (
    let t = dispatch_msgs 1 t in
    match Network.find_path origin dest time t.p.network with 
    | None -> (* no path *) (drop_msgs_nopath 1 t, None)
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
    ("table", `Assoc ([
      ("packets dispatched", `Int t.data.msgsent);
      ("packets received", `Int t.data.msgrecv);
      ("packets dropped due to node failure", `Int t.data.msgdrop_nodst);
      ("packets dropped due to partition or hub failure", `Int t.data.msgdrop_nopath);
      ("packets inflight", `Int t.data.msgflight);
      ("number of servers", `Int t.data.servers);
      ("number of clients", `Int t.data.clients);
      ("number of startups", `Int t.data.startup);
      ("number of failures", `Int t.data.failures);
      ("number of recoveries", `Int t.data.recover);
      ] @ 
      match t.data.latency with
      | [] -> []
      | _ -> [
        ("average latency", `Int (average t.data.latency));
        ("min latency", `Int (min t.data.latency));
        ("max latency", `Int (max t.data.latency));
      ]));
    ("figures", `List []);
    ("extra info", `Assoc [
      ("termination reason", `String (term_to_string t.data.reason));
      ]);
    ]
