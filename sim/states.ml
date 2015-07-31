open Common 

type ('s,'c) state = Server of 's | Client of 'c

type ('s,'c) t = (id * ('s,'c) state list) list

(* initial server and client state, servers will be numbers 1 to servers and clients servers+1 to servers+clients  *)
let rec init ~server_init ~client_init servers clients = 
  let total = servers+clients in
  let rec state = function
  | n when n>total -> []
  | n when n>servers -> (n, [Client (client_init n)]) :: state (n+1) 
  | n -> (n, [Server (server_init n)]) :: (state (n+1)) in
  state 1

let get n t = List.hd (List.assoc n t)

let clients t = map_filter (function (_,(Client c)::_) -> Some c | _ -> None)

let set_server n state_maybe t =
  match state_maybe with
  | None -> t
  | Some new_state ->
    let old_states = List.assoc n t in
    (n, (Server new_state) :: old_states) :: (List.filter (fun (i,ss) -> not (i==n)) t)

let set_client n state_maybe t =
  match state_maybe with
  | None -> t
  | Some new_state ->
    let old_states = List.assoc n t in
    (n, (Client new_state) :: old_states) :: (List.filter (fun (i,ss) -> not (i==n)) t)