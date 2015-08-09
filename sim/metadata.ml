open Common


let term = {
    name = "termination";
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

let workload_min = {
    name = "workload_min";
    sname = "wm";
    doc = "minimum time between requests from each client";
    default = None;
    min = Some 0;
    max = None;
  }

let workload_max = {
    name = "workload_max";
    sname = "wm";
    doc = "maximum time between requests from each client";
    default = None;
    min = Some 0;
    max = None;
  }