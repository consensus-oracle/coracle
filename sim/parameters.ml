open Metadata

exception Sanity_check_failure of string

(* only the input parameters from JSON *)
type t = {
  n: int;
  loss: float;
  term: int;
  seed: int option;
  }

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

let check_sanity (input:t) = 
  check_parameter input.n n;
(*   check_parameter input.loss loss; *)
  check_parameter input.term term
