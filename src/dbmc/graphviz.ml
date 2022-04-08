open Core

module Palette = struct
  let int_of_rgb r g b = (r * 256 * 256) + (g * 256) + b
  let white = int_of_rgb 255 255 255
  let light = int_of_rgb 200 200 200
  let dark = int_of_rgb 100 100 100
  let black = int_of_rgb 0 0 0
  let red = int_of_rgb 255 0 0
  let light_purple = int_of_rgb 203 201 226
  let red_orange = int_of_rgb 253 190 133
  let blue = int_of_rgb 0 0 255
  let cyan = int_of_rgb 0 200 200
  let lime = int_of_rgb 0 255 0

  let greens_with_alert =
    [
      red_orange;
      int_of_rgb 204 236 230;
      int_of_rgb 153 216 201;
      int_of_rgb 102 194 164;
      int_of_rgb 44 162 95;
      int_of_rgb 0 109 44;
    ]
end
[@@warning "-32"]

module C = struct
  (* string *)
  let str = [ `Color 127 ]

  (* vertice *)
  let pending = [ `Color Palette.red ]
  let mismatch = [ `Color Palette.red ]
  let alert = [ `Penwidth 1.5; `Style `Filled; `Fillcolor Palette.red ]

  let node_picked_set =
    [ `Penwidth 1.5; `Style `Filled; `Fillcolor Palette.light_purple ]

  let node_picked_get n =
    let ci =
      Int.clamp_exn n ~min:0 ~max:(List.length Palette.greens_with_alert)
    in
    let c = List.nth_exn Palette.greens_with_alert ci in
    [
      `Penwidth 1.5;
      (* `Style `Solid; *)
      (* `Color 127; *)
      `Style `Filled;
      `Fillcolor c;
    ]

  let node_unpicked =
    [ `Penwidth 0.5; `Color Palette.light ] @ [ `Style `Dashed ]

  (* Palette.black *)
  (* match (graph_vertex.picked_from_root, graph_vertex.picked) with
     | true, true -> [ `Penwidth 2.0 ]
     | false, true -> [ `Color Palette.cyan ]
     | true, false -> [ `Color Palette.light_red ]
     | false, false -> [ `Penwidth 0.5; `Color Palette.light ] *)

  (* edge *)

  let picked_dangling = [ `Arrowhead `Dot; `Color Palette.light ]
  let picked_from_root = [ `Color Palette.black ]
  let not_picked = [ `Arrowhead `Odot; `Color Palette.light ]
  let incomplete = [ `Style `Dashed ]
end

module Graph_node = struct
  type t = (Node.Node.t, string) Either.t [@@deriving compare, equal]

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

(* type vertex_info = {
     block_id : Id.t;
     rule_name : string;
     picked_from_root : bool;
     picked : bool;
   }

   type passing_state = {
     picked_from_root : bool;
     picked : bool;
     prev_vertex : Node.t option;
   } *)

let escape_gen_align_left =
  String.Escaping.escape_gen_exn
    ~escapeworthy_map:
      [
        ('{', '{'); ('}', '}'); ('\n', 'l'); ('<', '<'); ('>', '>'); ('"', '"');
      ]
    ~escape_char:'\\'

let label_escape s = Staged.unstage escape_gen_align_left s

let name_escape s =
  s
  |> String.substr_replace_all ~pattern:"~" ~with_:"_"
  |> String.substr_replace_all ~pattern:";" ~with_:"__"

module type GS = sig
  val state : Global_state.t
  val testname : string option
  val model : Z3.Model.model option
end

module DotPrinter_Make (S : GS) = struct
  let source_map = Lazy.force S.state.source_map

  let graph_of_gate_tree () =
    let root = S.state.root_node in
    let g = G.create () in

    let at_node (tree_node : Node.t ref) =
      G.add_vertex g (Either.first !tree_node)
    in
    let acc_f parent node =
      if Node.equal_ref node root
      then node
      else
        let edge_info =
          Edge_label.
            { picked_from_root = false; picked = false; complete = false }
        in
        (* G.add_edge g (Either.first !parent) (Either.first !node); *)
        G.add_edge_e g
          (Either.first !parent, Either.first edge_info, Either.first !node) ;
        node
    in
    ignore @@ Node.traverse_node ~at_node ~init:root ~acc_f root ;
    g

  module DotPrinter = Graph.Graphviz.Dot (struct
    include G

    let vertex_name vertex =
      Either.value_map vertex
        ~first:(fun (v : Node.t) -> Lookup_key.to_string v.key |> name_escape)
        ~second:(Fmt.str "\"%s\"")

    let graph_attributes (_g : t) =
      let graph_title =
        match S.testname with Some s -> [ `Label s ] | None -> []
      in
      [
        `Fontname "Consolas"; `Fontsize 16; `OrderingOut; `Rankdir `TopToBottom;
      ]
      @ graph_title

    let default_vertex_attributes _ = [ `Shape `Record ]

    let vertex_attributes v0 =
      let node_attr (node : Node.t) =
        let open Node in
        let rule = Node.rule_name node.rule in
        let key_value =
          match S.model with
          | None -> None
          | Some model ->
              let lookup_name = Lookup_key.to_string node.key in
              Logs.info (fun m -> m "lookup (to model) : %s" lookup_name) ;
              Solver.SuduZ3.(get_value model (var_s lookup_name))
        in
        let c_id =
          match node.rule with
          | Para_local _ | Para_nonlocal _ | Pending | Cond_choice _ ->
              node.block_id
          | _ -> node.key.x
        in
        let clause =
          Odefa_ast.Ast.Ident_map.Exceptionless.find c_id source_map
        in
        let content =
          let phis_string =
            ""
            (* Option.value_map (Hashtbl.find S.state.phi_map node.key) ~default:""
               ~f:(fun phi -> phi |> Z3.Expr.to_string |> label_escape) *)
            (* List.map phis ~f:(fun phi -> phi |> Constraint.show |> label_escape)
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
          let pvar = Global_state.pvar_picked S.state node.key in
          let outputs =
            Global_state.Unroll.get_messages S.state.unroll node.key
            |> List.map ~f:(fun m -> m.from)
          in
          Fmt.str
            "{ {[%s] | %a} | %a | %a | %s | {φ | { %s %s } } | %B | {out | %a \
             } | %s}"
            (Lookup_stack.to_string (Lookup_key.lookups node.key))
            (Fmt.option Solver.pp_value)
            key_value
            (Fmt.option Odefa_ast.Ast_pp_graph.pp_clause)
            clause Rstack.pp node.key.r_stk
            (Rstack.to_string node.key.r_stk)
            phis_string phi_status pvar (Fmt.Dump.list Id.pp) outputs rule
        in
        let styles =
          match node.rule with
          | Pending -> C.pending
          | Mismatch -> C.mismatch
          | _ ->
              if Hash_set.mem S.state.lookup_alert node.key
              then C.alert
              else if Riddler.is_picked S.model node.key
              then
                let is_defining_node =
                  List.length node.key.xs = 0 && Id.equal c_id node.key.x
                in
                if is_defining_node
                then
                  (* Fmt.pr "@[Fetch  Set at %a@]\n" Lookup_key.pp node.key; *)
                  C.node_picked_set
                else
                  (* C.node_picked_get (Hashtbl.find_exn S.state.node_get node.key) *)
                  let i =
                    Hashtbl.find_or_add S.state.node_get
                      (Lookup_key.drop_xs node.key)
                      ~default:(Fn.const 0)
                  in
                  (* Fmt.pr "@[Fetch  Get at %a@] = %d\n" Lookup_key.pp node.key i; *)
                  C.node_picked_get i
              else C.node_unpicked
        in

        [ `Label content ] @ styles
      in
      let string_attr s = [ `Label s ] @ C.str in
      Either.value_map v0 ~first:node_attr ~second:string_attr

    let default_edge_attributes _ = []

    let edge_attributes e =
      let edge_label (edge : Edge_label.edge) =
        let styles =
          if edge.complete
          then
            match (edge.picked, edge.picked_from_root) with
            | true, false -> C.picked_dangling
            | true, true -> C.picked_from_root
            | false, false -> C.not_picked
            | false, true -> failwith "no complete but picked"
          else C.incomplete
        in
        let labels = [ `Label "" ] in
        styles @ labels
      in
      let string_label s = [ `Label s ] in
      Either.value_map (E.label e) ~first:edge_label ~second:string_label

    let get_subgraph vs =
      Either.value_map vs
        ~first:(fun (v : Node.t) ->
          let sg_name =
            Printf.sprintf "%s_%s" (Id.show v.block_id)
              (Rstack.to_string v.key.r_stk)
          in
          (* CHANGE v.1.8.4 add 'sg_parent = None' to obtain subgraphs whose parents are the main graph) *)
          let (subgraph : Graph.Graphviz.DotAttributes.subgraph) =
            { sg_name; sg_attributes = [ `Label "" ]; sg_parent = None }
          in
          Some subgraph)
        ~second:(fun _ -> None)
  end)

  let output_graph () =
    let graph = graph_of_gate_tree () in
    let oc = Log.dot_file_oc_of_now () in
    DotPrinter.output_graph oc graph ;
    Out_channel.close oc
end

let output_graph ~model ~testname (state : Global_state.t) =
  let module GI = (val (module struct
                         let state = state
                         let testname = Some testname
                         let model = model
                       end) : GS)
  in
  let module Graph_dot_printer = DotPrinter_Make (GI) in
  Graph_dot_printer.output_graph ()