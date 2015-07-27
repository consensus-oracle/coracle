let election_timer_min = {
    name = "election_timer_min";
    sname = "etm";
    doc = "Raft's election timer is generated uniformly between the min and max values";
    default = Some 150;
    min = Some 0;
    max = None;
  }

let election_timer_max = {
    name = "election_timer_max";
    sname = "etm";
    doc = "Raft's election timer is generated uniformly between the min and max values";
    default = Some 300;
    min = Some 0; (*actually it must be more than election timer min *)
    max = None;
  }

 let heartbeat_interval = {
    name = "heartbeat_interval";
    sname = "h";
    doc = "How regularly a Raft leader broadcasts a heartbeat AppendEntries RPC to all other nodes";
    default = Some 100;
    min = Some 0;
    max = None;
  }