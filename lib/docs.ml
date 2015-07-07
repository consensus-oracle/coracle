
(* project metadata *)
let name = "coracle"
let doc = "Coracle: a consensus algorithm simulator for heterogeneous networks"
let bugs =  "https://github.com/consensus-oracle/coracle/issues"
let version = "0.1"

(* general *)
open Cmdliner
let man = [ `S "BUGS"; `P ("bug reports to "^bugs);]
let info = Term.info name ~version ~doc ~man