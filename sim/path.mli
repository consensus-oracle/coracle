open Common

type edge = id * id * int

(* true indicates that nodes is able to route *)
type node = (id * bool)

type t

val bellman_ford: int -> edge list -> node list -> t

val find_path: id -> id -> t -> int option

(* TODO: turn to string *)
val string_of_path: t -> unit