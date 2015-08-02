open Common
open Yojson.Safe

type node_type = Server | Client | Hub

let parse_node_type (json:json) :node_type = 
  json 
  |> function `String s -> s
  |> String.lowercase
  |> function
    | "server" -> Server
    | "client" -> Client
    | "hub" -> Hub

type link_type = Small | Medium | Large

let parse_link_type (json:json) :link_type = 
  json 
  |> function `String s -> s
  |> String.lowercase
  |> function
    | "s" -> Small
    | "m" -> Medium
    | "l" -> Large

let type_to_latency = function
  | Small -> 5
  | Medium -> 10 
  | Large -> 20

type node = {
	node_type: node_type;
	id: id;
}

let parse_node (json:json) :node =
  json 
  |> function `Assoc config -> {
    node_type = json_assoc "type" config |> parse_node_type;
    id = json_assoc "id" config |> function `Int i -> i;
  }

let find_node_type (nodes: node list) id = 
  (List.find (fun n -> n.id=id) nodes).node_type


type link = {
  src: id;
  dst: id;
  id: id;
}

let parse_link (json:json) :link =
  json 
  |> function `Assoc config -> {
    src = (json_assoc "start" config |> function `Int i -> i);
    dst = (json_assoc "end" config |> function `Int i -> i);
    id = (json_assoc "id" config |> function `Int i -> i);
  }

let get_endpoints link_id links= 
  let link = List.find (fun link -> link_id==link.id) links in
  (link.src,link.dst)

type link_event = {
  id: id;
  link_type: link_type;
  active: bool;
}

let parse_link_event (json:json) :link_event =
  json 
  |> function `Assoc config -> {
    id = (json_assoc "id" config |> function `Int i -> i);
    link_type = (json_assoc "type" config |> parse_link_type);
    active = (json_assoc "active" config |> function `Bool b -> b);
  }

type node_event = {
  id: id;
  active: bool;
}

let parse_node_event (json:json) :node_event =
  json 
  |> function `Assoc config -> {
    id = (json_assoc "id" config |> function `Int i -> i);
    active = (json_assoc "active" config |> function `Bool b -> b);
  }

type event = {
  time: time;
  links: link_event list;
  nodes: node_event list; 
}

let parse_event (json:json) :event =
  json 
  |> function `Assoc config -> {
    time = (json_assoc "time" config |> function `Int i -> i);
    links = (json_assoc "links" config |> function `List lst -> lst |> List.map parse_link_event);
    nodes = (json_assoc "nodes" config |> function `List lst -> lst |> List.map parse_node_event);
  }


type t = {
  nodes: node list;
  links: link list;
  events: event list;
  paths: (time * Path.t) list;
}

let parse_section name item_parser sections = 
  json_assoc name sections
  |> function `List lst -> lst
  |> List.map item_parser

let generate_edges links (link_events: link_event list) =
  List.filter (fun (link_event:link_event) -> link_event.active) link_events
  |> List.map (fun (link_event:link_event) -> 
      let (src,dst) = get_endpoints link_event.id links in
       (src, dst, type_to_latency link_event.link_type))

let generate_nodes (nodes: node list) =
  List.map (fun n -> 
    match n.node_type with
    | Server | Client -> (n.id,false)
    | Hub -> (n.id,true)) 
  nodes

let generate_paths (nodes: node list) (links:link list) (events: event list) =
  List.map (fun event -> 
    (event.time, Path.bellman_ford (List.length nodes) (generate_edges links event.links) (generate_nodes nodes))) 
  events


let parse (json:json) = 
  match json with
  | `Assoc config -> 
    let nodes = (
      json_assoc "nodes" config
      |> function `List lst -> lst
      |> List.map parse_node) in
    let links = (
      json_assoc "links" config
      |> function `List lst -> lst
      |> List.map parse_link) in
    let events = (
      json_assoc "events" config
      |> function `List lst -> lst
      |> List.map parse_event) in
    let paths = 
      generate_paths nodes links events in
  {nodes;links;events;paths}

let rec find_recent_event time events =
  (* we assume events is sorted as order is preserved since inputted JSON *)
  match events with
  | [] -> assert false
  | [x] -> x
  | x::y::zs -> 
    match time >= x.time with
    | true -> (* x is a past state *)
      match time < y.time with
      | true -> x 
      | false -> find_recent_event time (y::zs)
    | false -> (* we don't have a past state *)
      assert false

let rec find_recent_path time paths =
  (* we assume events is sorted as order is preserved since inputted JSON *)
  match paths with
  | [] -> assert false
  | [(tx,x)] -> x
  | (tx,x)::(ty,y)::zs -> 
    match time >= tx with
    | true -> (* x is a past state *)
      match time < ty with
      | true -> x
      | false -> find_recent_path time ((ty,y)::zs)
    | false -> (* we don't have a past state *)
      assert false

let find_node id time t =
  find_recent_event time t.events
  |> fun event -> List.find (fun node -> node.id==id) event.nodes
  |> fun node_event -> node_event.active

let find_active time (n1,n2) = 
  match n1.active, n2.active with
  | false, true -> Some (n1.id,time)
  | _ -> None

let rec zip xl yl =
  match xl,yl with
  | x::xs,y::ys -> (x,y) :: (zip xs ys)
  | [], [] -> []
  | _, [] | [], _ -> assert false

(* we are assuming event and nodes within events are ordered and all nodes are specificed *)
let rec find_recovery2 (t:t) (events: event list) = 
  match events with
  | e1::e2::es -> (
    zip e1.nodes e1.nodes
    |> map_filter (find_active e2.time)
    |> List.filter (fun (id,_) -> match (find_node_type t.nodes id) with Server -> true | _ -> false))
    :: find_recovery2 t (e2::es)
  | [_] -> []
  | [] -> []

let find_recovery t = List.flatten (find_recovery2 t t.events)

let find_path src dst time t =
  Path.find_path src dst (find_recent_path time t.paths)

let count_servers t = 
  t.nodes
  |> List.filter (fun node -> match node.node_type with Server -> true | _ -> false )
  |> List.length

let count_clients t = 
  t.nodes
  |> List.filter (fun node -> match node.node_type with Client -> true | _ -> false )
  |> List.length