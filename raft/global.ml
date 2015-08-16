open Common
open Yojson.Safe
open Json_basic

type pkt_counter = {
	arg_snd: int;
	arg_rcv: int;
	res_snd: int;
	res_rcv: int;
}

type modes = {
	f: int;
	c: int;
	l: int;
}

type t = {
	time: time;
	id:id;
	ae: pkt_counter;
	rv: pkt_counter;
	cl: pkt_counter;
	first_leader: time option;
	ele_start: int;
	ele_won: int;
	ele_restart: int;
	ele_stepdown: int;
	cmd_rcv: int;
	cmd_dsp: int;
	failure: modes;
	terms: (id * time * term) list;
	modes: (id * time * int) list
}

let init_pkt = {
  arg_snd = 0;
	arg_rcv = 0;
	res_snd = 0;
	res_rcv = 0;
}

let init_modes = 
  {f=0; c=0; l=0}

let init = {
	time = 0;
	id=0;
	ae = init_pkt;
  rv = init_pkt;
  cl = init_pkt;
	first_leader = None;
	ele_start = 0;
	ele_won = 0;
	ele_restart = 0;
	ele_stepdown = 0;
	cmd_rcv = 0;
	cmd_dsp = 0;
	failure = init_modes;
	terms=[];
	modes = []
}

let set_state time id g = {g with time=time; id=id}
let get_time g = g.time

let pkt_counter_to_json c name =
	[
	(name^" received", `Int (c.arg_rcv+c.res_rcv)); 
	(name^" dispatched", `Int (c.arg_snd+c.res_snd));
	(name^" requests received", `Int c.arg_rcv); 
	(name^" requests dispatched", `Int c.arg_snd);
	(name^" responses received", `Int c.res_rcv); 
	(name^" responses dispatched", `Int c.res_snd);
	]

let to_json g = 
	let term_updates = triple_to_doubles g.time g.terms in
	let mode_updates = triple_to_doubles g.time g.modes in
	let max_term = max_y_of_data term_updates in
	`Assoc [
		("table", `Assoc (
			(pkt_counter_to_json g.ae "append entries") @
			(pkt_counter_to_json g.rv "request votes") @ 
			(pkt_counter_to_json g.cl "client request") @ [
			("time to first leader", match g.first_leader with None -> `String "no leader" | Some t -> `Int t);
			("elections started", `Int g.ele_start);
			("elections won", `Int g.ele_won);
			("elections lost due to insuffient votes", `Int g.ele_restart);
			("elections lost due to step down", `Int g.ele_stepdown);
			("elections lost due to candidate failure", `Int g.failure.c);
			("highest term", `Int max_term);
			("number of commands received", `Int g.cmd_rcv);
			("number of commands dispatched in AppendEntries", `Int g.cmd_dsp);
			("number of node failures", `Int (g.failure.f+g.failure.c+g.failure.l));
			("number of leader failures", `Int g.failure.l);
			]));
		("figures", `List [
			figure_in_json
				~title:"Changes in local term number over time"
				~x_axis:"Time" ~x_start:0 ~x_end:g.time
				~y_axis:"Term number" ~y_start:0 ~y_end:max_term
				~legand:"Server ID's" ~lines:(List.length term_updates)
				(data_in_json term_updates);
			figure_in_json
				~title:"Changes in local mode over time"
				~x_axis:"Time" ~x_start:0 ~x_end:g.time
				~y_axis:"Mode (0=failed, 1=follower, 2=candidate, 3=leader)" ~y_start:0 ~y_end:3
				~legand:"Server ID's" ~lines:(List.length mode_updates)
				(data_in_json mode_updates);
			]);
		("extra info", `Assoc [
			("termination time", `Int g.time);
			]);
	]

let update_pkt_counter tick c = 
	match tick with
	| `ARG_RCV -> {c with arg_rcv = c.arg_rcv +1 }
	| `RES_RCV -> {c with res_rcv = c.res_rcv +1 }
	| `ARG_SND -> {c with arg_snd = c.arg_snd +1 }
	| `RES_SND -> {c with res_snd = c.res_snd +1 }

(* for terms and modes *)
(*  mode 0=failed, 1=follower, 2=candidate, 3=leader *)
let update_data new_point data t =
	match get_triple t.id data with
	| None -> (*first data point*)
		(t.id,t.time,new_point)::data
	| Some (_,_,old_point) -> (* needs fake extra point *)
		(t.id,t.time,new_point)::(t.id,t.time,old_point)::data


let update tick t = 
	match tick with 
	| `AE x -> {t with ae = (update_pkt_counter x t.ae)}
	| `RV x -> {t with rv = (update_pkt_counter x t.rv)}
	| `CL x -> {t with cl = (update_pkt_counter x t.cl)}
	| `ELE_WON -> (
		let t = {t with ele_won = t.ele_won +1 } in
		let first = match t.first_leader with
			| Some _ -> t.first_leader
			| None -> Some t.time in
		{t with
			first_leader = first;
			modes = update_data 3 t.modes t; } )
	| `ELE_START term -> {t with 
		ele_start = t.ele_start +1;
		terms = update_data term t.terms t;
		modes = update_data 2 t.modes t;
		}
	| `ELE_RESTART -> {t with ele_restart = t.ele_restart +1 }
	| `ELE_DOWN ->  {t with ele_stepdown = t.ele_stepdown +1 }
	| `CMD_RCV -> {t with cmd_rcv = t.cmd_rcv +1}
	| `CMD_DSP -> {t with cmd_dsp = t.cmd_dsp +1}
	| `FOLLOW term -> {t with 
			terms=update_data term t.terms t;
			modes=update_data 1 t.modes t; }
	| `FAIL -> {t with 
			modes=update_data 0 t.modes t; }

		

let rec update_n tick n t = 
	match n with
	| 0 -> t
	| 1 -> update tick t
	| n -> update_n tick (n-1) (update tick t)