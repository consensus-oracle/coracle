open Common
open Io
open Yojson.Safe


module type CONSENSUS = sig
  
	type config
	val parse_config: int -> int -> json -> config

	type global
	val reset_global: global
	val global_to_json: global -> json
	val set_state: time -> id -> global -> global

	type msg
	val msg_serialize: msg -> string
	val msg_deserialize: string -> msg
	val msg_to_json: msg -> json

	module type PROXY = sig
		type state
		val init: id -> config -> state
		val state_to_json: state -> json
		val eval: msg input -> state -> global -> state option * msg output list * global
	end

	module Server : PROXY
	module Client : PROXY

end
