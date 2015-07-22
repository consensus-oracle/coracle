open Yojson.Safe

val json_from_file: string -> json
val parameters_from_json: json -> Parameters.t
val get_protocol: string -> [> `Raft | `VRR | `Dummy ]
val proto_json_from_json: json -> json