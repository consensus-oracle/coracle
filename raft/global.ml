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
			("recieved", `Int g.ae_pkts_rcv); 
			("dispatched", `Int g.ae_pkts_snd);
		]);
		("request votes packets", `Assoc [
			("recieved", `Int g.rv_pkts_rcv); 
			("dispatched", `Int g.rv_pkts_snd);
		]);
		("time to first leader", match g.first_leader with None -> `String "no leader" | Some t -> `Int t);
		("number of elections", `Assoc [
			("started", `Int g.ele_start);
			("won", `Int g.ele_won);
			("lossed due to insuffient votes", `Int g.ele_restart);
			("lossed due to step down", `Int g.ele_stepdown);
		]);
	]
