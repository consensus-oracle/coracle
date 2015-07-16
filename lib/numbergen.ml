let init = function
  | None -> Random.self_init ()
  | Some seed -> Random.init seed

let maybe p = (Random.float 1.0 <= p)

let uniform  min max = Random.int (max-min) + min