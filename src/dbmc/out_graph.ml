open Core

module Node_vertex = struct
  type t = (Gate.t, string) Either.t [@@deriving compare, equal]

  let hash = Hashtbl.hash
end

module Edge_label = struct
  type edge_info = { cvar : string }

  and t = (edge_info, string) Either.t [@@deriving compare, equal]

  (* let compare = String.compare *)

  let default = Either.second ""
end

module Palette = struct
  let int_of_rgb r g b = (r * 256 * 256) + (g * 256) + b

  let white = int_of_rgb 255 255 255

  let light = int_of_rgb 200 200 200

  let dark = int_of_rgb 100 100 100

  let black = int_of_rgb 0 0 0

  let red = int_of_rgb 255 0 0

  let blue = int_of_rgb 0 0 255

  let lime = int_of_rgb 0 255 0
end

module G = Graph.Imperative.Digraph.ConcreteLabeled (Node_vertex) (Edge_label)

let node_rule_set = Hashtbl.create (module Lookup_key)

type graph_info_type = {
  source_map : Odefa_ast.Ast.clause Odefa_ast.Ast.Ident_map.t;
  block_map : Tracelet.t Odefa_ast.Ast.Ident_map.t;
  cvar_complete_map : (string, bool) Hashtbl.t;
  cvar_picked_map : (string, bool) Hashtbl.t;
}

let graph_info =
  ref
    {
      source_map = Odefa_ast.Ast.Ident_map.empty;
      block_map = Odefa_ast.Ast.Ident_map.empty;
      cvar_complete_map = Hashtbl.create (module String);
      cvar_picked_map = Hashtbl.create (module String);
    }

let graph_of_gate_tree tree =
  let g = G.create () in
  let counter = ref 0 in
  let add_node_edge ~cvar prev this =
    match (prev, cvar) with
    | None, _ -> G.add_vertex g (Either.first this)
    | Some prev, None -> G.add_edge g (Either.first prev) (Either.first this)
    | Some prev, Some cvar ->
        G.add_edge_e g
          (G.E.create (Either.first prev)
             (Either.first Edge_label.{ cvar })
             (Either.first this))
  in
  let add_terminal node ending =
    match node with
    | None -> ()
    | Some node ->
        Int.incr counter;
        G.add_edge g (Either.first node)
          (Either.second (Fmt.str "%s %d" ending !counter))
  in
  let rec loop_tree ?cvar ?(one_step = false) (prev : Gate.node option)
      (this : Gate.node) =
    let loop ?cvar prev this =
      if one_step then
        ()
      else
        match cvar with
        | Some cvar -> loop_tree ~cvar prev this
        | None -> loop_tree prev this
    in
    let add_node_rule ~key ~data =
      ignore @@ Hashtbl.add node_rule_set ~key ~data
    in
    let rule_name = Gate.rule_name this.rule in
    let data = rule_name in
    match this.rule with
    | Pending ->
        (* add_terminal prev "pending .." *)
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this
    | Proxy next ->
        (* add_node_rule ~key:this.key ~data; *)
        add_node_edge ~cvar prev !next
        (* loop_tree ~one_step:true (Some this) !next *)
    | Done _ ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this
    | Mismatch ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this
    | Discard next ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        loop (Some this) !next
    | Alias next ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        loop (Some this) !next
    | To_first next ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        loop (Some this) !next
    | Binop (n1, n2) ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        List.iter ~f:(fun n -> loop (Some this) !n) [ n1; n2 ]
    | Cond_choice (n1, n2) ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        List.iter ~f:(fun n -> loop (Some this) !n) [ n1; n2 ]
    | Callsite (nf, nb, _) ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        loop (Some this) !nf;
        let cvars = Gate.cvar_name_cores this in
        List.iter2_exn ~f:(fun n cvar -> loop ~cvar (Some this) !n) nb cvars
    | Condsite (nc, ncs) ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        loop (Some this) !nc;
        let cvars = Gate.cvar_name_cores this in
        List.iter2_exn ~f:(fun n cvar -> loop ~cvar (Some this) !n) ncs cvars
    | Para_local (np, _) ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        let cvars = Gate.cvar_name_cores this in
        List.iter2_exn
          ~f:(fun (n1, n2) cvar ->
            loop ~cvar (Some this) !n1;
            loop ~cvar (Some this) !n2)
          np cvars
    | Para_nonlocal (np, _) ->
        add_node_rule ~key:this.key ~data;
        add_node_edge ~cvar prev this;
        let cvars = Gate.cvar_name_cores this in
        List.iter2_exn ~f:(fun n cvar -> loop ~cvar (Some this) !n) np cvars
  in
  loop_tree None tree;
  g

module DotPrinter = Graph.Graphviz.Dot (struct
  include G

  let vertex_name vertex =
    Either.value_map vertex
      ~first:(fun (v : Gate.t) -> Fmt.str "\"%a\"" Lookup_key.pp v.key)
      ~second:(Fmt.str "\"%s\"")

  let graph_attributes _ = [ `Fontname "Consolas" ]

  let default_vertex_attributes _ = [ `Shape `Record ]

  let vertex_attributes v0 =
    let node_attr (node : Gate.t) =
      let open Gate in
      let rule = Hashtbl.find_exn node_rule_set node.key in
      let x, xs, r_stack = node.key in
      Logs.app (fun m ->
          m "lookup_key : %a \tblock_id : %a \trule_name : %a" Lookup_key.pp
            node.key Id.pp node.block_id Gate.pp_rule_name node.rule);
      let clause =
        let c_id =
          match node.rule with
          | Para_local _ | Para_nonlocal _ | Pending -> node.block_id
          | Proxy _ -> failwith "no proxy node"
          | _ -> x
        in
        Odefa_ast.Ast.Ident_map.find (Id.to_ast_id c_id) !graph_info.source_map
      in
      let content =
        Fmt.str "{%a | %a | %a | %s}" (Fmt.Dump.list Id.pp) (x :: xs)
          Relative_stack.pp r_stack Odefa_ast.Ast_pp_graph.pp_clause clause rule
      in
      let shape =
        match node.rule with
        | Pending -> Palette.lime
        | Mismatch -> Palette.red
        | _ -> Palette.black
      in
      [ `Label content; `Color shape ]
    in
    let string_attr s = [ `Label s; `Color 127 ] in
    Either.value_map v0 ~first:node_attr ~second:string_attr

  let default_edge_attributes _ = []

  let edge_attributes e =
    let edge_label edge =
      let cvar_core = edge.Edge_label.cvar in
      let complete = Hashtbl.find_exn !graph_info.cvar_complete_map cvar_core in
      let picked = Hashtbl.find_exn !graph_info.cvar_picked_map cvar_core in
      match (complete, picked) with
      | false, false -> [ `Arrowhead `Odot; `Color Palette.light ]
      | false, true -> failwith "no complete but picked"
      | true, false -> [ `Arrowhead `Dot ]
      | true, true -> [ `Color Palette.black ]
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
