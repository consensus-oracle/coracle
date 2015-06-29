open Common

val dispatcher: Lwt_unix.file_descr -> Io.output -> unit Lwt.t
val pass_to_raft: Lwt_unix.file_descr -> Io.input-> unit Lwt.t
val setup: int -> int -> unit Lwt.t