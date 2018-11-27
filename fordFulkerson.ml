open Graph
type flow_arc = {capacity: int; flow: int}

let string_of_flow_arc {capacity = c; flow = f} = Printf.sprintf "%d/%d" c f

let init_flow_graph graph = Graph.map graph (fun c -> {capacity = c; flow = 0})

let build_gap_graph graph =
    let map_graph = Graph.map graph (fun data -> data.flow) in
    let graph_ids = Graph.get_nodes_list graph in
    let rec build_reverse gap_graph graph_ids = match graph_ids with
        | [] -> gap_graph
        | id :: rest -> (let rec iter_arcs gap_graph arcs = match arcs with
                | [] -> gap_graph
                | (aid, adata) :: rest -> iter_arcs (Graph.add_arc gap_graph aid id (adata.capacity - adata.flow)) rest
        in
        build_reverse (iter_arcs gap_graph (Graph.out_arcs graph id)) rest)
    in
    build_reverse map_graph graph_ids

let calc_max_flow graph id = List.fold_left (fun acu (id, data) -> acu + data.flow) 0 (Graph.out_arcs id graph)