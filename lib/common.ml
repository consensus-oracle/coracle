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

let json_assoc_opt key js = 
  try Some (List.assoc key js) with Not_found -> None

let json_assoc key js =
  try List.assoc key js with Not_found -> raise (Json_parser_cannot_find_key key)

type cmd = int with sexp 
type outcome = Failure | Success of cmd with sexp

type msg = Cmd of cmd | Outcome of outcome 
  | CmdM of id * int * cmd | OutcomeM of id * int * outcome  | Startup

let cmd_to_json cmd = `Int cmd
let outcome_to_json = function
  | Failure -> `String "failure"
  | Success s -> cmd_to_json s

let msg_to_json = function
  | Cmd cmd -> `Assoc [
    ("type", `String "client command");
    ("command", cmd_to_json cmd );
    ]
  | CmdM (id,seq,cmd) -> `Assoc [
    ("type", `String "client command");
    ("client id", `Int id);
    ("seq #", `Int id);
    ("command", cmd_to_json cmd );
    ]
  | Outcome out -> `Assoc [
    ("type", `String "command response");
    ("response", outcome_to_json out);
    ]
  | OutcomeM (id,seq,out) -> `Assoc [
    ("type", `String "command response");
    ("client id", `Int id);
    ("seq #", `Int id);
    ("response", outcome_to_json out);
    ]
  | Startup -> `Assoc [
    ("type", `String "client startup");
    ]

let pull = function 
  | Some x -> x

let sum = List.fold_left (+) 0 

let average = function
  | [] -> 0
  | xs -> List.fold_left (+) 0 xs / List.length xs

let min ls = 
  let rec f n = function
  | [] -> n
  | x::xs when x<n -> f x xs
  | x::xs -> f n xs in
  f (List.hd ls ) ls

let max ls = 
  let rec f n = function
  | [] -> n
  | x::xs when x>n -> f x xs
  | x::xs -> f n xs in
  f (List.hd ls ) ls

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

let rec map_fold f t acc = function
  | [] -> (t, List.rev acc)
  | x::xs -> (
    let (t,y) = f t x in
    map_fold f t (y::acc) xs)

let rec update_triple (a,b,c) = function
  | [] -> [(a,b,c)]
  | (a1,_,_)::xs when a=a1 -> (a,b,c) :: xs
  | x::xs -> x :: (update_triple (a,b,c) xs)

let get_triple_exn x = 
  List.find (fun (a,b,c) -> a=x)

let get_triple x xs = 
  try Some (get_triple_exn x xs) with Not_found -> None

let get_value x xs = 
  try Some (List.assoc x xs) with Not_found -> None

let rec triple_to_doubles time lst =
  match lst with
  | [] -> []
  | (x,_,last_term)::_ -> 
    let (xs,rest) = List.partition (fun (a,b,c) -> a=x) lst in
    let xs_new = xs
      |> List.map (fun (_,b,c) -> (b,c))
      |> (fun ys -> (time,last_term)::ys) (* add fake end point *)
      |> List.stable_sort (fun (t1,m1) (t2,m2) -> 
        match compare t1 t2 with 
        | 0 -> compare m1 m2
        | n -> n ) in
    (x, xs_new) :: (triple_to_doubles time rest)
