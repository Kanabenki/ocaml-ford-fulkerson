open Graph
type flow_arc = {capacity: int; flow: int}

val string_of_flow_arc : flow_arc -> string

(* Take a graph, a source node, a destination node
 * then return the corresponding graph with the maximum flow *)
(* val run_ff : flow_arc graph -> id -> id -> flow_arc graph *)

val init_flow_graph : int graph -> flow_arc graph

val build_gap_graph : flow_arc graph -> int graph

(* Take a graph and its origin then return the maximum flow *)
val calc_max_flow : flow_arc graph -> id -> int
