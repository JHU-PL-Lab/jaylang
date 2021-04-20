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
  type t = (Gate.t, string) Either.t [@@deriving compare, equal]

  let hash = Hashtbl.hash
end

module Edge_label = struct
  type edge = { cvar : string option; picked_from_root : bool; picked : bool }

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
  (* the key is Lookup_key.t *)
  block_id : Id.t;
  rule_name : string;
  picked_from_root : bool;
  picked : bool;
  cvars_in : string list;
}

type edge_info = {
  (* the key is cvar : string *)
  picked : bool;
  picked_from_root : bool;
  complete : bool;
}

type graph_info_type = {
  source_map : Odefa_ast.Ast.clause Odefa_ast.Ast.Ident_map.t;
  block_map : Tracelet.t Odefa_ast.Ast.Ident_map.t;
  cvar_complete_map : (string, bool) Hashtbl.t;
  cvar_picked_map : (string, bool) Hashtbl.t;
  vertex_info_map : (Lookup_key.t, vertex_info) Hashtbl.t;
  edge_info_map : (string, edge_info) Hashtbl.t;
  phi_map : (Lookup_key.t, Constraint.t list) Hashtbl.t;
  phi_z3_map : (Lookup_key.t, Z3.Expr.expr list) Hashtbl.t;
  noted_phi_map : (Lookup_key.t, (string * Z3.Expr.expr) list) Hashtbl.t;
  model : Z3.Model.model ref option;
  testname : string option;
}

let graph_info =
  ref
    {
      source_map = Odefa_ast.Ast.Ident_map.empty;
      block_map = Odefa_ast.Ast.Ident_map.empty;
      cvar_complete_map = Hashtbl.create (module String);
      cvar_picked_map = Hashtbl.create (module String);
      vertex_info_map = Hashtbl.create (module Lookup_key);
      edge_info_map = Hashtbl.create (module String);
      phi_map = Hashtbl.create (module Lookup_key);
      phi_z3_map = Hashtbl.create (module Lookup_key);
      noted_phi_map = Hashtbl.create (module Lookup_key);
      model = None;
      testname = None;
    }

type passing_state = {
  picked_from_root : bool;
  picked : bool;
  prev_vertex : Gate.t option;
  prev_cvar : string option;
}

let add_option xs y =
  match y with
  | Some y -> if List.mem xs y ~equal:String.equal then xs else y :: xs
  | None -> xs

let graph_of_gate_tree tree =
  let g = G.create () in
  let add_node_edge prev_info this =
    match (prev_info.prev_vertex, prev_info.prev_cvar) with
    | None, None -> G.add_vertex g (Either.first this)
    | Some prev, None ->
        let edge_info =
          Edge_label.
            {
              cvar = None;
              picked_from_root = prev_info.picked_from_root;
              picked = true;
            }
        in
        G.add_edge_e g
          (G.E.create (Either.first prev) (Either.first edge_info)
             (Either.first this))
    | Some prev, Some cvar ->
        let picked = Hashtbl.find_exn !graph_info.cvar_picked_map cvar in
        let edge_info =
          Edge_label.
            {
              cvar = Some cvar;
              picked_from_root = prev_info.picked_from_root && picked;
              picked;
            }
        in
        G.add_edge_e g
          (G.E.create (Either.first prev) (Either.first edge_info)
             (Either.first this))
    | None, Some _ -> failwith "impossible"
  in
  let rec loop_tree ?(one_step = false) (prev_info : passing_state)
      (this : Gate.node) =
    (* state from passing prev_info *)
    let picked_cvar =
      match prev_info.prev_cvar with
      | Some cvar -> Hashtbl.find_exn !graph_info.cvar_picked_map cvar
      | None -> true
    in
    (* use && due to picked_from_root must be all true *)
    let picked_from_root = prev_info.picked_from_root && picked_cvar in
    let passing_info = { prev_info with picked_from_root } in
    (* recursive call pre-work *)
    let loop ?(next_one_step = false) ?cvar ?this next =
      if one_step then
        ()
      else
        let passing_state' =
          { passing_info with prev_vertex = this; prev_cvar = cvar }
        in
        loop_tree ~one_step:next_one_step passing_state' next
    in
    (* update graph node for this *)
    let add_or_update_graph_node tree_node =
      (* add or update the dict *)
      Hashtbl.update !graph_info.vertex_info_map tree_node.Gate.key ~f:(function
        | Some v ->
            {
              v with
              (* use || due to it can be tree if there exists such a all-true path *)
              picked_from_root = v.picked_from_root || picked_from_root;
              picked = v.picked || picked_cvar;
              cvars_in = add_option v.cvars_in prev_info.prev_cvar;
            }
        | None ->
            {
              block_id = this.block_id;
              rule_name = Gate.rule_name this.rule;
              picked_from_root;
              picked = picked_cvar;
              cvars_in = add_option [] prev_info.prev_cvar;
            });
      add_node_edge prev_info tree_node
    in
    (match this.rule with
    | To_visited _ -> ()
    | _ -> add_or_update_graph_node this);
    (* looping next *)
    let cvars = Gate.cvar_cores_of_node this in
    match this.rule with
    | Discard next | Alias next | To_first next -> loop ~this !next
    | Pending | Mismatch | Done _ -> ()
    | To_visited next -> loop_tree ~one_step:true prev_info !next
    (* | Proxy next -> loop ~next_one_step:true ~this !next *)
    | Binop (n1, n2) -> List.iter ~f:(fun n -> loop ~this !n) [ n1; n2 ]
    | Cond_choice (n1, n2) -> List.iter ~f:(fun n -> loop ~this !n) [ n1; n2 ]
    | Callsite (nf, nb, _) ->
        loop ~this !nf;
        List.iter2_exn ~f:(fun n cvar -> loop ~cvar ~this !n) nb cvars
    | Condsite (nc, ncs) ->
        loop ~this !nc;
        List.iter2_exn ~f:(fun n cvar -> loop ~cvar ~this !n) ncs cvars
    | Para_local (np, _) ->
        List.iter2_exn
          ~f:(fun (n1, n2) cvar ->
            loop ~cvar ~this !n1;
            loop ~cvar ~this !n2)
          np cvars
    | Para_nonlocal (np, _) ->
        List.iter2_exn
          ~f:(fun (n1, n2) cvar ->
            loop ~cvar ~this !n1;
            loop ~cvar ~this !n2)
          np cvars
    (* | Para_nonlocal (np, _) ->
        List.iter2_exn ~f:(fun n cvar -> loop ~cvar ~this !n) np cvars *)
  in

  let init_passing_state =
    {
      picked_from_root = true;
      picked = true;
      prev_vertex = None;
      prev_cvar = None;
    }
  in
  loop_tree init_passing_state tree;
  g

let escape_gen_align_left =
  String.Escaping.escape_gen_exn
    ~escapeworthy_map:
      [ ('{', '{'); ('}', '}'); ('\n', 'l'); ('<', '<'); ('>', '>') ]
    ~escape_char:'\\'

let dot_escaped s = Staged.unstage escape_gen_align_left s

module DotPrinter = Graph.Graphviz.Dot (struct
  include G

  let vertex_name vertex =
    Either.value_map vertex
      ~first:(fun (v : Gate.t) -> Fmt.str "\"%a\"" Lookup_key.pp v.key)
      ~second:(Fmt.str "\"%s\"")

  let graph_attributes _ =
    let graph_title =
      match !graph_info.testname with Some s -> [ `Label s ] | None -> []
    in
    [ `Fontname "Consolas"; `Fontsize 16 ] @ graph_title

  let default_vertex_attributes _ = [ `Shape `Record ]

  let vertex_attributes v0 =
    let node_attr (node : Gate.t) =
      let open Gate in
      let graph_vertex =
        Hashtbl.find_exn !graph_info.vertex_info_map node.key
      in
      let rule = graph_vertex.rule_name in
      let x, xs, r_stack = node.key in
      Logs.app (fun m ->
          m "lookup_key : %a \tblock_id : %a \trule_name : %a" Lookup_key.pp
            node.key Id.pp node.block_id Gate.pp_rule_name node.rule);
      let model = !(Option.value_exn !graph_info.model) in
      let key_value =
        let lookup_name = Constraint.name_of_lookup (x :: xs) r_stack in
        Logs.app (fun m -> m "lookup (to model) : %s" lookup_name);
        Solver_helper.Z3API.(get_value model (var_s lookup_name))
      in
      let clause =
        (* let block_id = Id.to_ast_id x in
           let block =
             Odefa_ast.Ast.(Ident_map.find block_id !graph_info.block_map)
           in
           let clause =
             match Tracelet.clause_of_x block x
        *)
        let c_id =
          match node.rule with
          | Para_local _ | Para_nonlocal _ | Pending | Cond_choice _ ->
              node.block_id
          | To_visited _ -> failwith "no proxy node"
          | _ -> x
        in
        Odefa_ast.Ast.Ident_map.Exceptionless.find (Id.to_ast_id c_id)
          !graph_info.source_map
      in
      let content =
        let phis =
          Option.value (Hashtbl.find !graph_info.phi_map node.key) ~default:[]
        in
        let phis_string =
          List.map phis ~f:(fun phi -> phi |> Constraint.show |> dot_escaped)
          |> String.concat ~sep:" | "
        in
        let phi_status =
          match Hashtbl.find !graph_info.noted_phi_map node.key with
          | Some [] -> ""
          | Some noted_phis ->
              let noted_vs =
                List.map noted_phis ~f:(fun (note, phi) ->
                    let phi_v =
                      Solver_helper.Z3API.(eval_value model phi |> bool_of_expr)
                    in
                    (note, phi_v))
              in
              Fmt.(
                str "| { %a }"
                  (list ~sep:(any " | ") (pair ~sep:(any ": ") string bool))
                  noted_vs)
          | None -> ""
        in
        Fmt.str "{ {[%s] | %a} | %a | %a | {φ | { %s %s } } | %s}"
          (Lookup_stack.mk_name (x :: xs))
          (Fmt.option Constraint.pp_value)
          key_value
          (Fmt.option Odefa_ast.Ast_pp_graph.pp_clause)
          clause Relative_stack.pp_chucked r_stack phis_string phi_status rule
      in
      let styles =
        match node.rule with
        | Pending -> [ `Color Palette.lime ]
        | Mismatch -> [ `Color Palette.red ]
        | _ -> (
            (* Palette.black *)
            match (graph_vertex.picked_from_root, graph_vertex.picked) with
            | true, true -> [ `Penwidth 2.0 ]
            | false, true -> [ `Color Palette.cyan ]
            | true, false -> [ `Color Palette.light_red ]
            | false, false -> [ `Penwidth 0.5; `Color Palette.light ])
      in
      [ `Label content ] @ styles
    in
    let string_attr s = [ `Label s; `Color 127 ] in
    Either.value_map v0 ~first:node_attr ~second:string_attr

  let default_edge_attributes _ = []

  let edge_attributes e =
    let edge_label (edge : Edge_label.edge) =
      let styles =
        match (edge.picked, edge.picked_from_root) with
        | false, false -> [ `Arrowhead `Odot; `Color Palette.light ]
        | false, true -> failwith "no complete but picked"
        | true, false -> [ `Arrowhead `Dot; `Color Palette.light ]
        | true, true -> [ `Color Palette.black ]
      in
      let name = Fmt.(str "%a" (option string) edge.cvar) in
      let labels = [ `Label name ] in
      styles @ labels
    in
    let string_label s = [ `Label s ] in
    Either.value_map (E.label e) ~first:edge_label ~second:string_label

  let get_subgraph _ = None
end)

let print_graph g = DotPrinter.output_graph Out_channel.stdout g

let output_graph g =
  let oc = Log.dot_file_oc_of_now () in
  DotPrinter.output_graph oc g;
  Out_channel.close oc

(* 
let%expect_test _ =
  let g = graph_of_gate_tree Debug_resource.n2 in
  print_graph g;
  [%expect
    {|
    digraph G {
      (x, [x; x], []);
      (x, [x; x; z], [+(x,g);+(x,f)]);


      (x, [x; x; z], [+(x,g);+(x,f)]) -> (x, [x; x], []);

      } |}] *)
