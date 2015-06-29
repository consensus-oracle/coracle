let () = Random.self_init ()

let maybe p = (Random.float 1.0 <= p)

let uniform  min max = Random.int (max-min) + min