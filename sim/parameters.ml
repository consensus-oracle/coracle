open Yojson

exception JSON_parsing_failure

type t = {
  n: int;
  loss: float;
  term: int;
  }

(* TODO: stub *)
 let of_json filename = 
   match Safe.from_file filename with
   | `Assoc [("nodes", `Int n);("loss", `Float loss); ("term", `Int term)] -> {n; term; loss}
   | _ -> raise JSON_parsing_failure