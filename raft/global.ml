open Common
open Yojson.Safe

type t = {
	ae_pkts_snd: int;
	ae_pkts_rcv: int;
	rv_pkts_snd: int;
	rv_pkts_rcv: int;
	first_leader: time option;
	ele_start: int;
	ele_won: int;
	ele_restart: int;
	ele_stepdown: int;
}

let init = {
	ae_pkts_snd = 0;
	ae_pkts_rcv = 0;
	rv_pkts_snd = 0;
	rv_pkts_rcv = 0;
	first_leader = None;
	ele_start = 0;
	ele_won = 0;
	ele_restart = 0;
	ele_stepdown = 0;
}

let to_json g = 
	`Assoc [
		("append entries packets", `Assoc [
			("received", `Int g.ae_pkts_rcv); 
			("dispatched", `Int g.ae_pkts_snd);
		]);
		("request votes packets", `Assoc [
			("received", `Int g.rv_pkts_rcv); 
			("dispatched", `Int g.rv_pkts_snd);
		]);
		("time to first leader", match g.first_leader with None -> `String "no leader" | Some t -> `Int t);
		("number of elections", `Assoc [
			("started", `Int g.ele_start);
			("won", `Int g.ele_won);
			("lost due to insuffient votes", `Int g.ele_restart);
			("lost due to step down", `Int g.ele_stepdown);
		]);
	]

let update tick t = 
	match tick with 
	| `AE_SND ->  {t with ae_pkts_snd = t.ae_pkts_snd +1 }
	| `AE_RCV ->  {t with ae_pkts_rcv = t.ae_pkts_rcv +1 }
	| `RV_SND -> {t with rv_pkts_snd = t.rv_pkts_snd +1 }
	| `RV_RCV -> {t with rv_pkts_rcv = t.rv_pkts_rcv +1 }
	| `ELE_WON -> {t with ele_won = t.ele_won +1 }
	| `ELE_START -> {t with ele_start = t.ele_start +1 }
	| `ELE_RESTART -> {t with ele_restart = t.ele_restart +1 }
	| `ELE_DOWN ->  {t with ele_stepdown = t.ele_stepdown +1 }

let rec update_n tick n t = 
	match n with
	| 0 -> t
	| 1 -> update tick t
	| n -> update_n tick (n-1) (update tick t)