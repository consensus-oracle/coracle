open Common

exception Src_dst_are_same
exception NoRoute of int * int
exception NoRouteDst of int

type item = 
 | NoPath 
 | Self
 | Path of int * id

let item_to_int = function
 | NoPath -> 999
 | Self -> 0
 | Path (v,_) -> v

type t = item array array

type edge = id * id * int

let iterate (t:t) (f: (int * int) -> item -> unit) = 
  Array.iteri (fun x col -> Array.iteri (fun y box -> f (x+1,y+1) box) col) t

let iterate_starts (t:t) (f: int -> item array -> unit) = 
  Array.iteri (fun x col -> f (x+1) col) t

let read t (src,dst) = try t.(src-1).(dst-1) with _ -> raise (NoRoute (src,dst))
let read_1d t index = try t.(index-1) with _ -> raise (NoRouteDst index)
let write t (src,dst) v = try Array.set t.(src-1) (dst-1) v with _ -> raise (NoRoute (src,dst))
let write_1d t index = try Array.set t (index-1) with _ -> raise (NoRouteDst index)

let print t = 
	iterate t (fun (x,y) v -> Printf.printf "(%i,%i) %i \n" x y (item_to_int v))

let relex_edge t (src,dst,weight) = 
	iterate_starts t (fun start endpoints -> 
		match read_1d endpoints src with 
		| NoPath -> (* no path to link source *) ()
		| Self -> write_1d endpoints dst (Path (weight,src)) 
		| Path (w_to_src,pred) -> 
			match read_1d endpoints dst with
			| NoPath -> write_1d endpoints dst (Path (weight+w_to_src,src)) 
			| Self -> (* loop *) ()
			| Path (w_to_dst,_) -> if (w_to_src+weight)<w_to_dst then write_1d endpoints dst (Path (weight+w_to_src,src)))

let bellman_ford n edges = 
  let paths = Array.make_matrix n n NoPath in
  (* set self *)
  iterate paths (fun (src,dst) _ -> if src=dst then write paths (src,dst) Self else ());
  for _ = 1 to n do
    List.iter (relex_edge paths) edges;
	done; paths 


let find_path src dst paths = 
	match read paths (src,dst) with
	| NoPath -> None
	| Self -> raise Src_dst_are_same
	| Path (weight,id) -> Some weight
