let init = function
  | None -> Random.self_init ()
  | Some seed -> Random.init seed

let maybe p = (Random.float 1.0 <= p)

let uniform  min max = Random.int (max-min) + min

type distribution = Fixed of int | Uniform of int * int

let generate = function
  | Fixed x -> x
  | Uniform (x,y) -> uniform x y

let to_distribution = function
  | None, None -> None
  | Some x, None -> Some (Fixed x)
  | None, Some x -> Some (Fixed x)
  | Some min, Some max -> Some (Uniform (min,max))