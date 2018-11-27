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


(* List of nodes defining a path in the graph *)
type path = id list

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
 *
 *  find_path : int graph -> id list -> id -> id -> path option
 *
 *)

let rec find_path residual forbidden id1 id2 =
    
    (* Loop on every arc of id1 *)
    let rec loop_on_arcs residual forbidden id1 id2 out_arcs_id1 =

    (* Boucler sur les arcs du node en cours d'étude : fonction à part? *)
    match out_arcs_id1 with
        | [] -> None
        | (id_next, cost) :: tail_list -> if (cost > 0 && !(List.mem id_next forbidden))

                                                (* Si le chemin est viable et que le noeud n'a pas déjà été franchi *)
                                                then 

                                                    if (id_next = id2)

                                                        (* On termine le chemin si on a atteint la destination *)
                                                        then Some (id2 :: forbidden)

                                                        (* On travaille sur le noeud suivant si ce n'est pas la destination
                                                            ... mais on oublie pas de retenir le noeud qu'on vient de quitter *)
                                                        else find_path residual (id1 :: forbidden) id_next id2

                                                else 
                                                    (* boucler sur le reste des arcs *)
                                                    loop_on_arcs residual forbidden id1 id2 tail_list

    in 
    
    let out_arcs_of_id1 = out_arcs residual id1 in

    loop_on_arcs residual forbidden id1 id2 out_arcs_of_id1
