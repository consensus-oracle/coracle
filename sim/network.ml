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

let json_of_node_type = function
  | Server -> `String "server"
  | Client -> `String "client"
  | Hub -> `String "hub"

type link_type = Small | Medium | Large
type direction = Bi | Uni

let parse_link_type (json:json) :link_type = 
  json 
  |> function `String s -> s
  |> String.lowercase
  |> function
    | "s" -> Small
    | "m" -> Medium
    | "l" -> Large

let json_of_link_type = function
  | Small -> `String "s"
  | Medium -> `String "m"
  | Large -> `String "l"

let parse_direction (json:json) :direction = 
  json 
  |> function `String s -> s
  |> String.lowercase
  |> function
    | "bi" -> Bi
    | "uni" -> Uni

let json_of_direction = function 
  | Bi -> `String "bi"
  | Uni -> `String "uni"

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

let json_of_node (node:node) =
  `Assoc [
    ("id", `Int node.id);
    ("type", json_of_node_type node.node_type);
    ]

let find_node_type (nodes: node list) id = 
  (List.find (fun n -> n.id=id) nodes).node_type


type link = {
  src: id;
  dst: id;
  id: id;
  direction: direction;
}

let parse_link (json:json) :link =
  json 
  |> function `Assoc config -> {
    src = (json_assoc "start" config |> function `Int i -> i);
    dst = (json_assoc "end" config |> function `Int i -> i);
    id = (json_assoc "id" config |> function `Int i -> i);
    direction = json_assoc "direction" config |> parse_direction;
  }

let json_of_link (link:link) =
  `Assoc [
    ("id", `Int link.id);
    ("start", `Int link.src);
    ("end", `Int link.dst);
    ("direction",json_of_direction link.direction);
    ]

let get_endpoints link_id links = 
  let link = List.find (fun link -> link_id==link.id) links in
  match link.direction with 
  | Uni -> [(link.src,link.dst)]
  | Bi -> [(link.src,link.dst); (link.dst,link.src)]


type link_event = {
  id: id;
  link_type: link_type;
  active: bool;
}

let parse_link_event (json:json) :link_event =
  json 
  |> function `Assoc config -> {
    id = (json_assoc "id" config |> function `Int i -> i);
    link_type = (json_assoc_def "type" config (`String "l") |> parse_link_type);
    active = (json_assoc "active" config |> function `Bool b -> b);
  }

let json_of_link_event (le:link_event) =
  `Assoc [
    ("id", `Int le.id);
    ("type", json_of_link_type le.link_type);
    ("active", `Bool le.active);
    ]

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

let json_of_node_event (ne:node_event) =
  `Assoc [
    ("id", `Int ne.id);
    ("active", `Bool ne.active);
    ]

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

let json_of_event (event:event) =
  `Assoc [
    ("time", `Int event.time);
    ("links", `List (List.map json_of_link_event event.links));
    ("nodes", `List (List.map json_of_node_event event.nodes));
    ]  

let combine crt nw =
  {time = nw.time;
  links = 
    List.fold_left (fun (links:link_event list) (update:link_event) -> 
      update :: (List.filter (fun (l:link_event) -> l.id<>update.id) links)) crt.links nw.links
    |> List.sort (fun (a:link_event) (b:link_event) -> compare a.id b.id );
  nodes = 
    List.fold_left (fun (nodes:node_event list) (update:node_event) -> 
      update :: (List.filter (fun l -> l.id<>update.id) nodes)) crt.nodes nw.nodes
    |> List.sort (fun (a:node_event) (b:node_event) -> compare a.id b.id );
  }

let rec fill_in (curr:event) (events:event list) = 
  match events with
  | [] -> []
  | x::xs -> 
    let new_curr = combine curr x in 
    new_curr :: fill_in new_curr xs


type t = {
  nodes: node list;
  links: link list;
  events: event list;
  paths: (time * Path.t) list;
}

let json_of_path (time,path) =
  `Assoc [
    ("time", `Int time);
   (* ("paths", `String (string_of_path path)); *)
    ]

let json_of_t (t:t) =
  `Assoc [
    ("nodes", `List (List.map json_of_node t.nodes));
    ("links", `List (List.map json_of_link t.links));
    ("event", `List (List.map json_of_event t.events));
    ("paths", `List (List.map json_of_path t.paths));
    ]

let parse_section name item_parser sections = 
  json_assoc name sections
  |> function `List lst -> lst
  |> List.map item_parser

let generate_edges links (link_events: link_event list) =
  List.filter (fun (link_event:link_event) -> link_event.active) link_events
  |> List.map (fun (link_event:link_event) -> 
      List.map (fun (src,dst) -> (src, dst, type_to_latency link_event.link_type)) 
      (get_endpoints link_event.id links)
      )
  |> List.flatten

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
      |> List.map parse_node) 
      |> List.sort (fun (a:node) (b:node) -> compare a.id b.id)in
    let links = (
      json_assoc "links" config
      |> function `List lst -> lst
      |> List.map parse_link)
      |> List.sort (fun (a:link) (b:link) -> compare a.id b.id)in
    let events = (
      json_assoc "events" config
      |> function `List lst -> lst
      |> List.map parse_event
      |> List.sort (fun a b -> compare a.time b.time)
      |> fun e -> fill_in (List.hd e) e)
      |> List.sort (fun a b -> compare a.time b.time) in
    let paths = 
      generate_paths nodes links events in
  let t = {nodes;links;events;paths} in
  (* pretty_to_channel stdout (json_of_t t); *)
  t

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

let find_active (n1,n2) = 
  assert (n1.id=n2.id);
  match n1.active, n2.active with
  | false, true -> Some n1.id
  | _ -> None

let find_inactive (n1,n2) = 
  assert (n1.id=n2.id);
  match n1.active, n2.active with
  | true, false -> Some n1.id
  | _ -> None

let rec zip xl yl =
  match xl,yl with
  | x::xs,y::ys -> (x,y) :: (zip xs ys)
  | [], [] -> []
  | _, [] | [], _ -> assert false

(* we are assuming event and nodes within events are ordered and all nodes are specificed *)
let rec find_rel_events (t:t) (events: event list) f = 
  assert (sorted (fun (e1:event) (e2:event) -> compare e1.time e2.time) events);
  match events with
  | e1::e2::es -> (
    zip e1.nodes e2.nodes
    |> map_filter f
    |> List.filter (fun id -> match (find_node_type t.nodes id) with Server -> true | _ -> false)
    |> List.map (fun id -> (id,e2.time)) )
    :: (find_rel_events t (e2::es) f)
  | [_] -> []
  | [] -> []

let find_recovery t = List.flatten (find_rel_events t t.events find_active)
let find_failure t = List.flatten (find_rel_events t t.events find_inactive) 


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