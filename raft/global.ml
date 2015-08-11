open Common
open Yojson.Safe

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

let figure_in_json ~title ~y_axis ~x_axis ~legand 
	~x_start ~y_start ~x_end ~y_end ~lines (data:json) =
	`Assoc [
		("title", `String title);
		("x axis", `Assoc [
			("label", `String x_axis);
			("start", `Int x_start);
			("end", `Int x_end);
			]);
		("y axis", `Assoc [
			("label", `String y_axis);
			("start", `Int y_start);
			("end", `Int y_end);
			]);
		("legand", `Assoc [
			("label", `String legand);
			("data sets", `Int lines);
			]);
		("data", data);
	]

(* convert a simple list of (x,y) coordinate to JSON *)
let simple_data_in_json (data: (int * int) list) =
	`List (List.map (fun (x,y) -> `Assoc [("x",`Int x); ("y",`Int y); ]) data)

(* convery a list of list of (x,y) cooridates to JSON *)
let data_in_json (data: (int * ((int * int) list)) list) = 
	`List (List.map (fun (line_id,xy) -> `Assoc [
					("line id", `Int line_id); 
					("data", simple_data_in_json xy)]) data)

let to_json g = 
	let term_updates = triple_to_doubles g.time g.terms in
	let mode_updates = triple_to_doubles g.time g.modes in
	let max_term = g.terms
		|> List.map (fun (id,time,term) -> term) 
		|> max in
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
			("lost due to candidate failure", `Int g.failure.c);
			("highest term", `Int max_term);
			]);
		("number of commands", `Assoc [
			("received", `Int g.cmd_rcv);
			("dispatched in AppendEntries", `Int g.cmd_dsp);
			]);
		("number of node failures", `Assoc [
			("total", `Int (g.failure.f+g.failure.c+g.failure.l));
			("leader failure", `Int g.failure.l);
			]);
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
	| `CMD_RCV -> {t with cmd_rcv = t.cmd_rcv +1}
	| `CMD_DSP -> {t with cmd_dsp = t.cmd_dsp +1}
	| `TERM term -> 
		match get_triple t.id t.terms with
		| None -> {t with terms=(t.id,t.time,term)::t.terms}
		| Some (_,_,old_term) -> {t with terms=(t.id,t.time,term)::(t.id,t.time,old_term)::t.terms}

let rec update_n tick n t = 
	match n with
	| 0 -> t
	| 1 -> update tick t
	| n -> update_n tick (n-1) (update tick t)