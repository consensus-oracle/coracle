open Common 

type 'state t = 'state list list

let rec init f p = 
  let max = Parameters.(p.n) in
  let rec state = function
  | 0 -> [] 
  | n -> [f (create_nodes max n 0)] :: state (n-1) in
  state max

let get n t = List.hd (List.nth t (int_of_id n))


let set n state_maybe t =
  match state_maybe with
  | None -> t
  | Some n_state ->
    let update m m_states = 
      if m=(int_of_id n) then n_state :: m_states else m_states in
    List.mapi update t 