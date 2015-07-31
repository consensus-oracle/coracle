open Sexplib.Conv

type id = int with sexp
type index = int with sexp
type term = int with sexp

let ident x = x
let string_of_id = string_of_int
let int_of_id = ident
let id_of_int = ident

type time = int 
type span = int 

let to_span ms = ms 
let to_time = to_span
let compare_time = compare
let sec_of_span = float_of_int 
let incr x y = x+y
let int_of_time = ident


let string_of_option f = function 
  | None -> "none" 
  | Some x -> f x

let rec add_unique y = function
  | [] -> [y]
  | x::xs when x=y -> x::xs
  | x::xs -> x :: (add_unique y xs)

let to_option f x = 
  try Some (f x) with _ -> None

let rec split_from char str last = 
  match to_option (String.index_from str last) char with
  | Some next ->
	let substring = String.sub str last (next-last) in
	substring :: (split_from char str (next+1))
  | None -> []

let split char str = split_from char str 0

let rec merge char = function
  | x::xs -> x ^ (Char.escaped char) ^ (merge char xs)
  | [] -> ""

 let rec string_of_list f = function
  | x::xs -> (f x) ^ (string_of_list f xs)
  | [] -> ""

(* given min id, max id and node id, create_nodes returns a list of peers 
  e.g. create_nodes 3 1 1 returns 2 3 *)
let rec create_nodes max me now =
	if now>max then []
	else if now=me then create_nodes max me (now+1)
    else now :: (create_nodes max me (now+1))

exception Not_implemented of string

let bugs_to = "bug reports to https://github.com/consensus-oracle/coracle/issues"

(* metadata for each simulation parameter *)
type 'a parameter = {
  name: string;
  sname: string;
  doc: string;
  default: 'a option;
  min: 'a option;
  max: 'a option;
}

exception Sanity_check_failure of string

let check_parameter x meta =
  match meta.min with
  | None -> (* no min to check *) ()
  | Some m when m<=x -> (* value is above min *) ()
  | Some m -> raise (Sanity_check_failure (
    Printf.sprintf "%s is %i, it must above (or equal to) %i." meta.name x m));
  match meta.max with 
  | None -> (* no max to check *) ()
  | Some m when m>=x -> (* value is below max *) ()
  | Some m -> raise (Sanity_check_failure (
    Printf.sprintf "%s is %i, it must below (or equal to) %i." meta.name x m))

open Yojson.Safe

exception Json_parser_cannot_find_key of string

let json_assoc key js =
  try List.assoc key js with Not_found -> raise (Json_parser_cannot_find_key key)

type cmd = int 
type outcome = Failure | Success of cmd

type msg = Cmd of cmd | Outcome of outcome | Startup

let pull = function Some x -> x

let average = function
  | [] -> 0
  | xs -> List.fold_left (+) 0 xs / List.length xs

let rec map_filter f = function
  | [] -> []
  | x::xs -> (
    match f x with 
    | None -> map_filter f xs 
    | Some y -> y :: (map_filter f xs))

let rec map_filter_fold f t acc = function
  | [] -> (t, List.rev acc)
  | x::xs -> (
    match f t x with 
    | (t, None) -> map_filter_fold f t acc xs
    | (t, Some y) -> map_filter_fold f t (y::acc) xs)
