open Sexplib.Conv

type id = int with sexp
type index = int with sexp
type term = int with sexp

let ident x = x
let string_of_id = string_of_int
let int_of_id = ident
let id_of_int = ident

type time = int (* milliseconds *)
type span = int (* milliseconds *)

let to_span ?s:(s=0) ms = ms + (1000*s)
let to_time = to_span
let compare_time = compare
let sec_of_span n = float_of_int n /. 1000.0
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


let rec create_nodes max me now =
	if now=max then []
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
