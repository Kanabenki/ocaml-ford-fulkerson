open Graph
type flow_arc = {capacity: int; flow: int}

val string_of_flow_arc : flow_arc -> string

val run_ff : flow_arc graph -> flow_arc graph
val init_flow_graph : int graph -> flow_arc graph
val calc_max_flow : 'a graph -> int
