open Core

module Palette = struct
  let int_of_rgb r g b = (r * 256 * 256) + (g * 256) + b

  let white = int_of_rgb 255 255 255

  let light = int_of_rgb 200 200 200

  let dark = int_of_rgb 100 100 100

  let black = int_of_rgb 0 0 0

  let red = int_of_rgb 255 0 0

  let light_red = int_of_rgb 120 0 0

  let blue = int_of_rgb 0 0 255

  let cyan = int_of_rgb 0 200 200

  let lime = int_of_rgb 0 255 0
end

module Graph_node = struct
  type t = (Gate.Node.t, string) Either.t [@@deriving compare, equal]

  let hash = Hashtbl.hash
end

module Edge_label = struct
  type edge = { picked_from_root : bool; picked : bool; complete : bool }

  and t = (edge, string) Either.t [@@deriving compare, equal]

  let default = Either.second ""
end

module G = Graph.Imperative.Digraph.ConcreteLabeled (Graph_node) (Edge_label)

(* why vertex_info in module Graph_node doesn't work?
     Without a property dict, it's hard to update a node via comparing it's key.
     Say node A and node B shares the same lookup key, but doesn't have the same picked key
     If node A is added to the graph first, then node B is ignored (or B overrides A).
     There is unrealistic to think OCamlGraph.Dot can merge these two nodes.
     Therefore, we have to merge on our own, which occurs outside the module.
*)
type vertex_info = {
  block_id : Id.t;
  rule_name : string;
  picked_from_root : bool;
  picked : bool;
}

type passing_state = {
  picked_from_root : bool;
  picked : bool;
  prev_vertex : Gate.Node.t option;
}

let escape_gen_align_left =
  String.Escaping.escape_gen_exn
    ~escapeworthy_map:
      [ ('{', '{'); ('}', '}'); ('\n', 'l'); ('<', '<'); ('>', '>') ]
    ~escape_char:'\\'

let dot_escaped s = Staged.unstage escape_gen_align_left s

module type Graph_state = sig
  val state : Search_tree.state

  val testname : string option

  val model : Z3.Model.model option

  val source_map : Odefa_ast.Ast.clause Odefa_ast.Ast.Ident_map.t
end

module DotPrinter_Make (S : Graph_state) = struct
  let graph_of_gate_tree () =
    let root = S.state.root_node in
    let g = G.create () in

    let at_node (tree_node : Gate.Node.t ref) =
      G.add_vertex g (Either.first !tree_node)
    in
    let acc_f parent node =
      if Gate.Node_ref.equal node root then
        node
      else
        let edge_info =
          Edge_label.
            { picked_from_root = false; picked = false; complete = false }
        in
        (* G.add_edge g (Either.first !parent) (Either.first !node); *)
        G.add_edge_e g
          (Either.first !parent, Either.first edge_info, Either.first !node);
        node
    in
    ignore @@ Gate.traverse_node ~at_node ~init:root ~acc_f root;
    g

  module DotPrinter = Graph.Graphviz.Dot (struct
    include G

    let vertex_name vertex =
      Either.value_map vertex
        ~first:(fun (v : Gate.Node.t) -> Fmt.str "\"%a\"" Lookup_key.pp v.key)
        ~second:(Fmt.str "\"%s\"")

    let graph_attributes _ =
      let graph_title =
        match S.testname with Some s -> [ `Label s ] | None -> []
      in
      [ `Fontname "Consolas"; `Fontsize 16 ] @ graph_title

    let default_vertex_attributes _ = [ `Shape `Record ]

    let vertex_attributes v0 =
      let node_attr (node : Gate.Node.t) =
        let open Gate in
        let rule = Gate.Node.rule_name node.rule in
        let xxs = Lookup_key.lookups node.key in
        let key_value =
          match S.model with
          | None -> None
          | Some model ->
              let lookup_name = Lookup_key.to_str node.key in
              Logs.info (fun m -> m "lookup (to model) : %s" lookup_name);
              Solver.SuduZ3.(get_value model (var_s lookup_name))
        in
        let clause =
          let c_id =
            match node.rule with
            | Para_local _ | Para_nonlocal _ | Pending | Cond_choice _ ->
                node.block_id
            | _ -> node.key.x
          in
          Odefa_ast.Ast.Ident_map.Exceptionless.find c_id S.source_map
        in
        let content =
          let phis_string =
            Option.value_map (Hashtbl.find S.state.phi_map node.key) ~default:""
              ~f:(fun phi -> phi |> Z3.Expr.to_string |> dot_escaped)
            (* List.map phis ~f:(fun phi -> phi |> Constraint.show |> dot_escaped)
               |> String.concat ~sep:" | " *)
          in

          let phi_status =
            match Hashtbl.find S.state.noted_phi_map node.key with
            | Some [] -> ""
            | Some noted_phis -> (
                match S.model with
                | Some model ->
                    let noted_vs =
                      List.map noted_phis ~f:(fun (note, phi) ->
                          let phi_v =
                            Solver.(
                              SuduZ3.eval_value model phi |> SuduZ3.unbox_bool)
                          in
                          (note, phi_v))
                    in
                    Fmt.(
                      str "| { %a }"
                        (list ~sep:(any " | ")
                           (pair ~sep:(any ": ") string bool))
                        noted_vs)
                | None -> "")
            | None -> ""
          in
          Fmt.str "{ {[%s] | %a} | %a | %s | {φ | { %s %s } } | (%d) | %s}"
            (Lookup_stack.str_of_t xxs)
            (Fmt.option Constraint.pp_value)
            key_value
            (Fmt.option Odefa_ast.Ast_pp_graph.pp_clause)
            clause
            (Rstack.str_of_t node.key.r_stk)
            phis_string phi_status (List.length node.preds) rule
        in
        let picked =
          Option.value_map S.model ~default:false ~f:(fun model ->
              Option.value
                (Solver.SuduZ3.get_bool model (Riddler.pick_at_key node.key))
                ~default:true)
        in
        let styles =
          match node.rule with
          | Pending -> [ `Color Palette.lime ]
          | Mismatch -> [ `Color Palette.red ]
          | _ ->
              (* Palette.black *)
              (* match (graph_vertex.picked_from_root, graph_vertex.picked) with
                 | true, true -> [ `Penwidth 2.0 ]
                 | false, true -> [ `Color Palette.cyan ]
                 | true, false -> [ `Color Palette.light_red ]
                 | false, false -> [ `Penwidth 0.5; `Color Palette.light ] *)
              if picked then
                [ `Penwidth 2.0 ]
              else
                [ `Penwidth 0.5; `Color Palette.light ]
        in

        let line_styles =
          (* node.has_complete_path *)
          if picked then [] else [ `Style `Dashed ]
        in
        [ `Label content ] @ styles @ line_styles
      in
      let string_attr s = [ `Label s; `Color 127 ] in
      Either.value_map v0 ~first:node_attr ~second:string_attr

    let default_edge_attributes _ = []

    let edge_attributes e =
      let edge_label (edge : Edge_label.edge) =
        let styles =
          if edge.complete then
            match (edge.picked, edge.picked_from_root) with
            | false, false -> [ `Arrowhead `Odot; `Color Palette.light ]
            | false, true -> failwith "no complete but picked"
            | true, false -> [ `Arrowhead `Dot; `Color Palette.light ]
            | true, true -> [ `Color Palette.black ]
          else
            [ `Style `Dashed ]
        in
        let labels = [ `Label "" ] in
        styles @ labels
      in
      let string_label s = [ `Label s ] in
      Either.value_map (E.label e) ~first:edge_label ~second:string_label

    let get_subgraph _ = None
  end)

  let output_graph () =
    let graph = graph_of_gate_tree () in
    let oc = Log.dot_file_oc_of_now () in
    DotPrinter.output_graph oc graph;
    Out_channel.close oc
end
