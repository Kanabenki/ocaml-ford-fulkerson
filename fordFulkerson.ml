type flow_arc = {capacity: int; flow: int}

let string_of_flow_arc {capacity = c; flow = f} = Printf.sprintf "%d/%d" c f

let init_flow_graph graph = Graph.map graph (fun c -> {capacity = c; flow = 0})