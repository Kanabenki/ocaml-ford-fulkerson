open Graph
open Printf
    
type path = string

(* Format of text files: lines of the form 
 *
 *  v id               (node with the given identifier)
 *  e label id1 id2    (arc with the given (string) label. Goes from node id1 to node id2.)
 *
 *)


(* Reads a line with a village node. *)
let read_village_node graph line =
  try Scanf.sscanf line "v %s %s" (fun id demand -> add_arc (add_node graph id) id "t" demand)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with a factory node. *)
let read_factory_node graph line =
  try Scanf.sscanf line "f %s %s" (fun id production -> add_arc (add_node graph id) "s" id production)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with a road between a factory and a village. *)
let read_road graph line =
  try Scanf.sscanf line "r %s %s %s" (fun idF idV capacity -> add_arc graph idF idV capacity)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"



let init_circulation_graph = add_node (add_node empty_graph "s") "t"


let from_roads_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : 
            - v for a village and its demand (example : v v1 5)
            - f for a factory and its production capacity (example : f f1 3)
            - r for a road between a factory and a village (example : r f1 v1 4)
        *)
        else match line.[0] with
          | 'v' -> read_village_node graph line
          | 'f' -> read_factory_node graph line
          | 'r' -> read_road graph line
          | _ -> graph
      in                 
      loop graph2        
    with End_of_file -> graph
  in

  let final_graph = loop init_circulation_graph in
  
  close_in infile ;
  final_graph

let check_solution graph =
  let rec loop nodes = match nodes with
    | id :: rest -> (match find_arc graph id "t" with
      (* We use parsing to avoid rewriting things in the FF module *)
      | Some s -> if Scanf.sscanf s "%d/%d" (fun a b -> a = b) then loop rest else false
      | None -> loop rest)
    | [] -> true
  in
  loop (get_nodes_list graph)

let export_solution path graph =
  (* Open a write-file. *)
  let ff = open_out path in

  fprintf ff (if check_solution graph then "# The solution is valid\n" else "# !!! The solution is not valid !!!\n");

  (* Declare the graph. *)
  fprintf ff "digraph G {\n" ;

  (* Force display from left to right *)
  fprintf ff "rankdir=LR;\n" ;

  (* Write all arcs *)
  v_iter graph (fun id out -> List.iter (fun (id2, lbl) -> fprintf ff "%s -> %s [ label = \"%s\" ];\n" id id2 lbl) out) ;
  
  (* End of file *)
  fprintf ff "}\n" ;
  
  close_out ff ;
  ()