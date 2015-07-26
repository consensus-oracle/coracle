open Common 

type 'state t = (id *'state list) list

let rec init f p = 
  let max = Parameters.(Network.count_servers p.network) in
  let rec state = function
  | n when n>max  -> [] 
  | n -> (n, [f (create_nodes max n 1)]) :: state (n+1) in
  state 1

let get n t = List.hd (List.assoc n t)


let set n state_maybe t =
  match state_maybe with
  | None -> t
  | Some new_state ->
    let old_states = List.assoc n t in
    (n, new_state :: old_states) :: (List.filter (fun (i,ss) -> not (i==n)) t)