open Yojson

exception JSON_parsing_failure

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
