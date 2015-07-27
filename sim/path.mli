open Common

type edge = id * id * int

type t

val bellman_ford: int -> edge list -> t

val find_path: id -> id -> t -> int option