open Common
open Rpcs
open Io
open Yojson.Safe


type state = {
  id: id;
  last_leader: id option;
  servers: int;
  outstanding: (int * cmd) option;
  seq_num: int;
  timeout: int option;
}

let try_next state = 
  match state.last_leader with
  | Some id -> id
  | None -> Numbergen.uniform 1 state.servers

let init id (config:State.config) = {
  id=id;
  last_leader=None;
  servers = config.servers;
  outstanding =None;
  seq_num = 1;
  timeout = config.client_timer;
}

let state_to_json state : json = 
  `Assoc [
    ("type", `String "state update");
    ("data", `Assoc [
      ("last leader", match state.last_leader with None -> `String "none" | Some id -> `Int id);
      ("outstanding", match state.outstanding with None -> `String "none" | Some (id,cmd) -> `Int id);
      ("next sequence number", `Int state.seq_num);
    ]);
  ]

let receive_timeout timer state global = 
  match timer with
  | Client n -> 
    let (seq_num,cmd) = pull state.outstanding in
    let pkt = CRA ClientArg.({seq_num;cmd}) in
    let state = {state with last_leader=None} in
    (Some state,
      [PacketDispatch (try_next state,pkt); 
      SetTimeout (pull state.timeout,Client state.seq_num);
      ],Global.update (`CL `ARG_SND) global)

let receive_client_request_reply id (pkt:ClientRes.t) state global = 
  let global = Global.update (`CL `RES_RCV) global in
   match state.outstanding with
    | None -> (* ignore it *) (None,[],global)
    | Some (n,_) when n<>pkt.seq_num -> (* ignore it *) (None,[],global)
    | Some (seq_num,cmd) -> (
      match pkt.success with 
      | Some result -> 
        (Some {state with outstanding=None; last_leader=Some id},
          [LocalDispatch (Outcome result); CancelTimeout (Client seq_num)],global)
      | None -> 
        let new_pkt = CRA ClientArg.({seq_num;cmd}) in
        let state = {state with last_leader= pkt.leader_hint} in
        (Some state,[PacketDispatch (try_next state, new_pkt); ResetTimeout (pull state.timeout,Client seq_num)],
        Global.update (`CL `ARG_SND) global))

let recieve_client_request client_cmd (state:state) global = 
  match state.outstanding with
  | None ->
    let pkt = CRA ClientArg.({seq_num = state.seq_num; cmd = client_cmd}) in
      (Some {state with outstanding = Some (state.seq_num, client_cmd); seq_num=state.seq_num+1},
       [PacketDispatch (try_next state,pkt);
        SetTimeout (pull state.timeout,Client state.seq_num)],
      Global.update (`CL `ARG_SND) global)
  | Some _ -> (* can only had one request at a time *)
    (None, [LocalDispatch (Outcome Failure)],global)