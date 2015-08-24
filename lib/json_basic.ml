open Common
open Yojson.Safe

let figure_in_json ~title ~y_axis ~x_axis ~legand 
	~x_start ~y_start ~x_end ~y_end ~lines (data:json) : json=
	`Assoc [
		("title", `String title);
		("x axis", `Assoc [
			("label", `String x_axis);
			("start", `Int x_start);
			("end", `Int x_end);
			]);
		("y axis", `Assoc [
			("label", `String y_axis);
			("start", `Int y_start);
			("end", `Int y_end);
			]);
		("legand", `Assoc [
			("label", `String legand);
			("data sets", `Int lines);
			]);
		("data", data);
	]

(* convert a simple list of (x,y) coordinate to JSON *)
let simple_data_in_json (data: (int * int) list) =
	`List (List.map (fun (x,y) -> `Assoc [("x",`Int x); ("y",`Int y); ]) data)

(* convery a list of list of (x,y) cooridates to JSON *)
let data_in_json (data: (int * ((int * int) list)) list) = 
	`List (List.map (fun (line_id,xy) -> `Assoc [
					("line id", `Int line_id); 
					("data", simple_data_in_json xy)]) data)

let max_y_of_data data = 
	data
	|> List.map (fun (_,lst) -> lst |> List.map (fun (_,y) -> y) |> max) 
	|> max

(* given a list of (x,y), create the fill in corridates and termination point *)
let rec fill_points term_x (data: (int * int) list) =
	match data with
	| (x1,y1)::(x2,y2)::rest -> (x1,y1) :: (x2,y1) :: (fill_points term_x ((x2,y2)::rest))
	| (x1,y1)::[] -> [(x1,y1); (term_x,y1)]
	| [] -> []