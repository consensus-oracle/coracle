open Common
open Yojson.Safe

type node_type = Server | Client | Hub

let parse_node_type (json:json) :node_type = 
  json 
  |> function `String s -> s
  |> function
    | "server" -> Server
    | "client" -> Client
    | "hub" -> Hub

type link_type = Small | Medium | Large

let parse_link_type (json:json) :link_type = 
  json 
  |> function `String s -> s
  |> function
    | "s" -> Small
    | "m" -> Medium
    | "l" -> Large

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
  events: event list
}

let parse_section name item_parser sections = 
  json_assoc name sections
  |> function `List lst -> lst
  |> List.map item_parser

let parse (json:json) = 
  match json with
  | `Assoc config -> {
    nodes = (
      json_assoc "nodes" config
      |> function `List lst -> lst
      |> List.map parse_node);
    links = (
      json_assoc "links" config
      |> function `List lst -> lst
      |> List.map parse_link);
    events = (
      json_assoc "events" config
      |> function `List lst -> lst
      |> List.map parse_event);
  }

let rec find_recent_event time events : event =
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

let find_node id time t =
  find_recent_event time t.events
  |> fun event -> List.find (fun node -> node.id==id) event.nodes
  |> fun node_event -> node_event.active

let find_latency = function
  | Small -> 5 
  | Medium -> 20 
  | Large -> 100

let find_path src dst time t =
  (* TODO: we assume 1 hop only *)
  let link_static = List.find (fun link -> link.src=src && link.dst=dst) t.links in
  let event = find_recent_event time t.events in
  let link_dyn = List.find (fun (link:link_event) -> link.id==link_static.id) event.links in
  match link_dyn.active with
  | true -> Some (find_latency link_dyn.link_type) 
  | false ->  None

let count_servers t = 3