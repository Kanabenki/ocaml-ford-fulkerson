open Graph

type flow_arc = {capacity: int; flow: int}

let string_of_flow_arc {capacity = c; flow = f} = Printf.sprintf "%d/%d" f c

let init_flow_graph graph = Graph.map graph (fun c -> {capacity = c; flow = 0})

let build_gap_graph graph =
    let map_graph = Graph.map graph (fun data -> (data.capacity - data.flow)) in
    let graph_ids = Graph.get_nodes_list graph in
    let rec build_reverse gap_graph graph_ids = match graph_ids with
        | [] -> gap_graph
        | id :: rest -> (let rec iter_arcs gap_graph arcs = match arcs with
                | [] -> gap_graph
                | (aid, adata) :: rest -> iter_arcs (Graph.add_arc gap_graph aid id adata.flow) rest
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
 *  Must be used like this at initialization :
 *  >>> find_path residual [] source dest
 *
*)



let rec find_path residual forbidden id1 id2 =

  (* Loop on every arc of id1 *)
  let rec loop_on_arcs residual forbidden id1 id2 out_arcs_id1 =

    (* Loop on every arcs starting from the current node *)
    match out_arcs_id1 with
      | [] -> None (* Each arc has been tested : no path founded *)
      | (id_next, cost) :: tail_list -> if (cost > 0 && not (List.mem id_next forbidden))

          (* If we can get to the next node ... *)
          then 

            if (id_next = id2)

            (* ... we return the path if we reach the destination node  *)
            then Some (List.rev (id2 :: forbidden))

            (* ... if it is not the destination node, we work on the next node 
             * and we save the node we're leaving in the forbidden list
             *
             * WARNING : if we do not find a path with this node, we must test the other arcs!*)
            else 
              let next = find_path residual (id1 :: forbidden) id_next id2
              in
                match next with
                  | None -> loop_on_arcs residual forbidden id1 id2 tail_list
                  | _ -> next

          else 
            (* boucler sur le reste des arcs *)
            loop_on_arcs residual forbidden id1 id2 tail_list

  in 

  let out_arcs_of_id1 = out_arcs residual id1 in

    loop_on_arcs residual forbidden id1 id2 out_arcs_of_id1




(* Evaluate the augmenting value according to the given residual network and augmenting path.
 *
 *
 * (We should check that find_arc gr id1 id2 only returns an arc FROM id1 TO id2
 *  and not an arc from id2 to id1 for whatever reason)
*)
let eval_augmenting_value residual path =

  (* Iterations *)
  (* We stop iterating when we checked the entire path and return the min_value.
   * Otherwise, we evaluate the possible new min_value with the following node in the path. *)
  let rec eval_aug_value flow_graph path min_val id1 = 
    match path with
      | [] -> min_val
      | id2 :: tl_list -> 

          (* Getting the arc from id1 to id2 *)
          let label = find_arc residual id1 id2 in

			(* Decapsulate the value in label :
				if this label is None, an error is raised *)
			match label with
				| None -> raise Not_found
				| Some value -> 

		        (* We save its label if it is lower than the old min_value *)
		        if (value < min_val) 
		        then eval_aug_value flow_graph tl_list value id2
		        else eval_aug_value flow_graph tl_list min_val id2

  in

    (* Initialization : we save the first node, set the min_value at 999999,
     * then start iterating *)
    match path with 
      | [] -> 0
      | id :: tl_list -> eval_aug_value residual tl_list (Int32.to_int Int32.max_int) id


let rec augment_flow_graph graph path value =match path with
 | id_s :: id_e :: rest -> (match Graph.find_arc graph id_s id_e with
    | Some flow -> Graph.add_arc graph id_s id_e {capacity=flow.capacity; flow=flow.flow + value}
    | None -> (match Graph.find_arc graph id_s id_e with
        | Some flow -> graph
        | None -> raise (Graph.Graph_error "Part of path missing in graph")))
  | _ :: [] ->  raise (Graph.Graph_error "Only one node in path")
  | [] -> graph

let run_ff graph id_start id_end =
  let rec loop graph =
    let gap_graph = build_gap_graph graph
    in
    match find_path gap_graph [] id_start id_end with
      | Some path -> let augm_value = eval_augmenting_value gap_graph path
          in
          if augm_value > 0 then loop (augment_flow_graph graph path augm_value) else graph
      | None -> graph
  in
  loop graph
