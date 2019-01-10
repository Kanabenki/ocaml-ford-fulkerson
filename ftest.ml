open Graph
open FordFulkerson

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* The source and sink node for the flow graph. *)
  and source = Sys.argv.(2)
  and sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

  (* Rewrite the graph that has been read then export a dot file for Graphviz. *)
  let () =
  let out_graph = Graph.map (FordFulkerson.run_ff (FordFulkerson.init_flow_graph (Graph.map graph int_of_string)) source sink) FordFulkerson.string_of_flow_arc
  in
  Gfile.write_file outfile out_graph;
  Gfile.export (outfile^".gv") out_graph;
  in

  ()


