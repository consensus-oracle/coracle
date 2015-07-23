open Common
open Metadata

(* only the input parameters from JSON *)
type t = {
  n: int;
  loss: float;
  term: int;
  seed: int option;
  }


let check_sanity (input:t) = 
  check_parameter input.n n;
(*   check_parameter input.loss loss; *)
  check_parameter input.term term
