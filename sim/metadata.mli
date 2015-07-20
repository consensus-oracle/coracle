(* metadata for each simulation parameter *)
type 'a parameter = {
  name: string;
  sname: string;
  doc: string;
  default: 'a option;
  min: 'a option;
  max: 'a option;
}

val n: int parameter
val loss: float parameter
val term: int parameter
val config_file: string parameter
val trace: bool parameter
val no_sanity: bool parameter
val seed: int parameter
val output_file: string parameter