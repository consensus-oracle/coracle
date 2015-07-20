open Yojson
open Metadata
open Parameters

exception JSON_parsing_failure

(* find the value associated with a key, return default if not found *)
let find_int meta assoc_list = 
  let rec check_value = function 
    | [] -> (match meta.default with
      | None -> raise JSON_parsing_failure
      | Some i -> i)
    | (k,`Int v)::_ when k=meta.name -> v
    | _::rest -> check_value rest in
  check_value assoc_list

let find_int_option meta assoc_list = 
  let rec check_value = function 
    | [] -> meta.default
    | (k,`Int v)::_ when k=meta.name -> Some v
    | _::rest -> check_value rest in
  check_value assoc_list

let find_float meta assoc_list = 
  let rec check_value = function 
    | [] -> (match meta.default with
      | None -> raise JSON_parsing_failure
      | Some i -> i)
    | (k,`Float v)::_ when k=meta.name -> v
    | _::rest -> check_value rest in
  check_value assoc_list


(* given a json file, return record of parameters *)
 let parameters_from_file filename = 
   match Safe.from_file filename with
   | `Assoc config -> {
      n = find_int n config; 
      loss = find_float loss config;
      term = find_int term config;
      seed = find_int_option seed config;
      }
   | _ -> raise JSON_parsing_failure