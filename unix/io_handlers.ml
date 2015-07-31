open Lwt
open Io
open Common
open Yojson.Safe

module UnixInterface = 
  functor (C: Protocol.CONSENSUS) -> struct

  let bufsz = 4096

  let state = ref None
  let global = ref None

  let set_state = function
  	| None -> ()
  	| Some new_state -> state := Some new_state
   
  let get_state () = match !state with
  | None -> assert false
  | Some s -> s

  let set_global = function
    | None -> ()
    | Some new_state -> global := Some new_state
   
  let get_global () = match !global with
  | None -> assert false
  | Some s -> s

  (* storing a list of cancelled timers until we can do this in lwt *)
  let cancelled_timers = ref []

  let cancel_timer t = 
    cancelled_timers := t :: !cancelled_timers;
    return ()

  let check_timer t =
    Printf.printf "checking timer%!";
    List.exists ((=) t) !cancelled_timers

  let rec dispatcher fd event = 
    to_channel stdout (output_to_json C.msg_to_json event);
    match event with
    | PacketDispatch (id,pkt) -> 
        let buf = Lwt_bytes.of_string (C.msg_serialize pkt) in
        Lwt_bytes.sendto fd buf 0 4 [] (Id.sockaddr_of_id id)
        >>= fun _ -> return ()
    | SetTimeout (n,t) -> 
        Lwt_unix.sleep (sec_of_span n)
        >>= fun () -> 
          (if check_timer t then return () else pass_to_raft fd (Timeout t))
    | CancelTimeout t -> 
        cancel_timer t

  and pass_to_raft fd event = 
    to_channel stdout (input_to_json C.msg_to_json event);
    let (s,e,g) = C.Server.eval event (get_state()) (get_global())in 
    set_state s;
    set_global (Some g);
    to_channel stdout (C.Server.state_to_json (get_state ()));
    Lwt_list.iter_p (dispatcher fd) e

  let process buf len dst fd = 
    let str = String.sub (Lwt_bytes.to_string buf) 0 4 in
    let event = C.msg_deserialize str in
    pass_to_raft fd (PacketArrival (Id.id_of_sockaddr dst,event))

  let listen fd =
    let cont = ref true in
    let bufs = Lwt_pool.create 16 
        (fun () -> return (Lwt_bytes.create bufsz)) in
    let _ =
      while_lwt !cont do
        Lwt_pool.use bufs
          (fun buf ->
             Lwt_bytes.recvfrom fd buf 0 bufsz []
             >>= fun (len, dst) -> process buf len dst fd
          )
      done
    in
    let t,u = Lwt.task () in
    Lwt.on_cancel t (fun () -> cont := false);
    t

  let setup id max config_file =
    Printf.printf "Starting up";
    let json = from_file config_file in
    set_state (Some (C.Server.init id (C.parse_config json)));
    let id = id_of_int id in
    let src = Id.sockaddr_of_id id in
    let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
    let () = Lwt_unix.bind fd src in
    pass_to_raft fd (Startup id)
    >>= fun () -> listen fd

end