open Yojson

exception JSON_parsing_failure
exception Sanity_check_failure of string

type t = {
  n: int;
  loss: float;
  term: int;
  }

let defaults = {
  n = 5;
  loss = 0.0;
  term = 500;
}

let min = {
  n = 2;
  loss = 0.0;
  term = 0;
}

let max = {
  n = 100;
  loss = 1.0;
  term = 3600;
}

let check_value_int min max name value =
  if value < min || value > max 
  then raise (Sanity_check_failure (
    Printf.sprintf "%s is %i, it must be between %i and %i." name value min max))
  else ()

let check_value_float min max name value =
  if value < min || value > max 
  then raise (Sanity_check_failure (
    Printf.sprintf "%s is %f, it must be between %f and %f." name value min max))
  else ()

let check_sanity input = 
  check_value_int min.n max.n "nodes" input.n;
  check_value_float min.loss max.loss "loss" input.loss;
  check_value_int min.term max.term "termination" input.term

(* find the value associated with a key, return default if not found *)
let find_int default key assoc_list = 
  let rec check_value = function 
    | [] -> default
    | (k,`Int v)::_ when k=key -> v
    | _::rest -> check_value rest in
  check_value assoc_list

let find_float default key assoc_list = 
  let rec check_value = function 
    | [] -> default
    | (k,`Float v)::_ when k=key -> v
    | _::rest -> check_value rest in
  check_value assoc_list


(* given a json file, return record of parameters *)
 let of_json filename = 
   match Safe.from_file filename with
   | `Assoc config -> {
      n = find_int defaults.n "nodes" config; 
      loss = find_float defaults.loss "loss" config;
      term = find_int defaults.term "termination" config;
      }
   | _ -> raise JSON_parsing_failure
