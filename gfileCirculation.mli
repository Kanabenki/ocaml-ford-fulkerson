(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as strings. *)
val from_roads_file: path -> string graph

(* Export a dot file corresponding to a solution from a given graph *)
val export_solution: path -> string graph -> unit