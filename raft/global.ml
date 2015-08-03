open Common
open Yojson.Safe

type pkt_counter = {
	arg_snd: int;
	arg_rcv: int;
	res_snd: int;
	res_rcv: int;
}

type t = {
	time: time;
	ae: pkt_counter;
	rv: pkt_counter;
	cl: pkt_counter;
	first_leader: time option;
	ele_start: int;
	ele_won: int;
	ele_restart: int;
	ele_stepdown: int;
}

let init_pkt = {
  arg_snd = 0;
	arg_rcv = 0;
	res_snd = 0;
	res_rcv = 0;
}


let init = {
	time = 0;
	ae = init_pkt;
  rv = init_pkt;
  cl = init_pkt;
	first_leader = None;
	ele_start = 0;
	ele_won = 0;
	ele_restart = 0;
	ele_stepdown = 0;
}

let set_time time g = {g with time=time}
let get_time g = g.time

let pkt_counter_to_json c =
	`Assoc [
		("total packets", `Assoc [
			("received", `Int (c.arg_rcv+c.res_rcv)); 
			("dispatched", `Int (c.arg_snd+c.res_snd));
		]);
		("request packets", `Assoc [
			("received", `Int c.arg_rcv); 
			("dispatched", `Int c.arg_snd);
		]);
		("response packets", `Assoc [
			("received", `Int c.res_rcv); 
			("dispatched", `Int c.res_snd);
		]);
	]

let to_json g = 
	`Assoc [
		("termination time", `Int g.time);
		("append entries packets", pkt_counter_to_json g.ae);
		("request votes packets", pkt_counter_to_json g.rv);
		("client packets", pkt_counter_to_json g.cl);
		("time to first leader", match g.first_leader with None -> `String "no leader" | Some t -> `Int t);
		("number of elections", `Assoc [
			("started", `Int g.ele_start);
			("won", `Int g.ele_won);
			("lost due to insuffient votes", `Int g.ele_restart);
			("lost due to step down", `Int g.ele_stepdown);
		]);
	]

let update_pkt_counter tick c = 
	match tick with
	| `ARG_RCV -> {c with arg_rcv = c.arg_rcv +1 }
	| `RES_RCV -> {c with res_rcv = c.res_rcv +1 }
	| `ARG_SND -> {c with arg_snd = c.arg_snd +1 }
	| `RES_SND -> {c with res_snd = c.res_snd +1 }

let update tick t = 
	match tick with 
	| `AE x -> {t with ae = (update_pkt_counter x t.ae)}
	| `RV x -> {t with rv = (update_pkt_counter x t.rv)}
	| `CL x -> {t with cl = (update_pkt_counter x t.cl)}
	| `ELE_WON -> 
		let t = {t with ele_won = t.ele_won +1 } in (
		match t.first_leader with
		| Some _ -> t
		| None -> {t with first_leader=Some t.time} )
	| `ELE_START -> {t with ele_start = t.ele_start +1 }
	| `ELE_RESTART -> {t with ele_restart = t.ele_restart +1 }
	| `ELE_DOWN ->  {t with ele_stepdown = t.ele_stepdown +1 }

let rec update_n tick n t = 
	match n with
	| 0 -> t
	| 1 -> update tick t
	| n -> update_n tick (n-1) (update tick t)