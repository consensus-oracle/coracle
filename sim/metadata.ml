open Common

let n = {
    name = "nodes";
    sname = "n";
    doc = "number of nodes to simulate";
    default = Some 5;
    min = Some 2;
    max = Some 100;
  }

let loss = {
    name = "loss";
    sname = "l";
    doc = "probability of message loss";
    default = Some 0.1;
    min = Some 0.0;
    max = Some 1.0;
  }

let term = {
    name = "term";
    sname = "t";
    doc = "number of time units to simulate";
    default = Some 100;
    min = Some 0;
    max = Some 3600;
  }

let config_file = {
    name = "file";
    sname = "f";
    doc = "name of config file";
    default = None;
    min = None;
    max = None;
}

let trace = {
    name = "trace";
    sname = "r";
    doc = "enable tracing";
    default = Some false;
    min = None;
    max = None;
}

let no_sanity = {
    name = "not_sanitized";
    sname = "ns";
    doc = "disable sanity check on input values";
    default = Some false;
    min = None;
    max = None;
}

let seed = {
    name = "seed";
    sname = "s";
    doc = "set a random seed for reproducibility";
    default = None;
    min = None;
    max = None;
}

let output_file = {
    name = "output_file";
    sname = "o";
    doc = "name of file to write output to";
    default = None;
    min = None;
    max = None;
}