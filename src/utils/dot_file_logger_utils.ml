open Batteries;;

include Dot_file_logger_utils_types;;

module Make(Base : Dot_file_logger_base) = struct
  include Base;;

  let logging_level = ref default_level;;

  let get_level () = !logging_level;;

  let set_level level = logging_level := level;;

  let log level name data =
    (* Make sure that we should be logging this action. *)
    if compare_level level (get_level ()) < 0
    then ()
    else
      let (nodes,edges) = graph_of data in
      let name = string_of_name name in
      let buffer = Buffer.create 1024 in
      Buffer.add_string buffer "strict digraph ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " {\n";
      Buffer.add_string buffer "  rankdir=\"LR\";\n";
      Buffer.add_string buffer "  label=\"";
      Buffer.add_string buffer name;
      Buffer.add_string buffer "\";\n";
      nodes
      |> Enum.iter
        (fun node ->
          Buffer.add_string buffer "  \"";
          Buffer.add_string buffer (string_of_dot_node_id node.dot_node_id);
          Buffer.add_string buffer "\"[style=filled";
          begin
            match node.dot_node_color with
            | Some color ->
              Buffer.add_string buffer ",fillcolor=\"";
              Buffer.add_string buffer color;
              Buffer.add_char buffer '"';
            | None -> ()
          end;
          begin
            match node.dot_node_text with
            | Some text ->
              Buffer.add_string buffer ",label=\"";
              Buffer.add_string buffer text;
              Buffer.add_char buffer '"';
            | None -> ()
          end;
          Buffer.add_string buffer "];\n"
        );
      edges
      |> Enum.iter
        (fun edge ->
          Buffer.add_string buffer "  \"";
          Buffer.add_string buffer (string_of_dot_node_id edge.dot_edge_source);
          Buffer.add_string buffer "\" -> \"";
          Buffer.add_string buffer (string_of_dot_node_id edge.dot_edge_target);
          Buffer.add_char buffer '"';
          begin
            match edge.dot_edge_text with
            | Some text ->
              Buffer.add_string buffer "[label=\"";
              Buffer.add_string buffer text;
              Buffer.add_string buffer "\",key=\"";
              Buffer.add_string buffer text;
              Buffer.add_string buffer "\"]";
            | None -> ()
          end;
          Buffer.add_string buffer ";\n";
        );
      Buffer.add_string buffer "}\n";
      let text = Buffer.contents buffer in
      let filename = name ^ ".dot" in
      Utils.set_file_contents filename text
  ;;
end;;