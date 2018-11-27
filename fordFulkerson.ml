open Graph

type flow_arc = {capacity: int; flow: int}

let string_of_flow_arc {capacity = c; flow = f} = Printf.sprintf "%d/%d" c f

let init_flow_graph graph = Graph.map graph (fun c -> {capacity = c; flow = 0})



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

            (* We save its label if it is lower than the old min_value *)
            if (label < min_val) 
            then eval_aug_value flow_graph tl_list label id2
            else eval_aug_value flow_graph tl_list min_val id2

  in

    (* Initialization : we save the first node, set the min_value at 999999,
     * then start iterating *)
    match path with 
      | [] -> 0
      | id :: tl_list -> eval_aug_value residual tl_list 999999 id
