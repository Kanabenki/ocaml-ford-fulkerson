open Graph
open FordFulkerson

let () =

  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  
  in

  (* Open file *)
  let graph = GfileCirculation.from_roads_file infile in

  (* Rewrite the graph that has been read then export a dot file for Graphviz. *)
  let () =
  let out_graph = Graph.map (FordFulkerson.run_ff (FordFulkerson.init_flow_graph (Graph.map graph int_of_string)) "s" "t") FordFulkerson.string_of_flow_arc
  in
  Gfile.write_file outfile out_graph;
  GfileCirculation.export_solution (outfile^".gv") out_graph;
  in

  ()


