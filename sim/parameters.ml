open Common
open Metadata

(* only the input parameters from JSON *)
type t = {
  term: int;
  seed: int option;
  network: Network.t;
  workload: Numbergen.distribution option;
  }


let check_sanity (input:t) = 
  check_parameter input.term term;
