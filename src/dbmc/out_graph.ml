open Core

(* module Search_edge = struct
  type t = Constraint.t list

  let compare = List.compare Constraint.compare

  let default = []
end *)

(* type node_type =
      | Pending
      | Proxy
      | Done
      | Mismatch
      | Discard
      | Alias
      | To_first
      | Binop
      | Cond_choice
      | Callsite
      | Condsite
      | Para_local
      | Para_nonlocal
    [@@deriving show { with_path = false }]

    type node_key = { block_id : Id.t; key : Lookup_key.t; node_type : node_type }

    type t = (node_key, string) Either.t

    let of_gate_node (gnode : Gate.t) : t =
      let block_id = gnode.block_id in
      let key = gnode.key in
      let node_type =
        match gnode.rule with
        | Gate.Pending -> Pending
        | Gate.Proxy _ -> Proxy
        | Gate.Done _ -> Done
        | Gate.Mismatch -> Mismatch
        | Gate.Discard _ -> Discard
        | Gate.Alias _ -> Alias
        | Gate.To_first _ -> To_first
        | Gate.Binop _ -> Binop
        | Gate.Cond_choice _ -> Cond_choice
        | Gate.Callsite _ -> Callsite
        | Gate.Condsite _ -> Condsite
        | Gate.Para_local _ -> Para_local
        | Gate.Para_nonlocal _ -> Para_nonlocal
      in
      { block_id; key; node_type } *)

module Search_vertex = struct
  type t = (Gate.t, string) Either.t [@@deriving compare, equal]

  let hash = Hashtbl.hash
end

module String_label = struct
  type t = string

  let compare = String.compare

  let default = ""
end

module Palette = struct
  let int_of_rgb r g b = (r * 256 * 256) + (g * 256) + b

  let white = int_of_rgb 255 255 255

  let black = int_of_rgb 0 0 0

  let red = int_of_rgb 255 0 0

  let blue = int_of_rgb 0 0 255

  let lime = int_of_rgb 0 255 0
end

module G =
  Graph.Imperative.Digraph.ConcreteLabeled (Search_vertex) (String_label)

let node_rule_set = Hashtbl.create (module Lookup_key)

let source_map = ref Odefa_ast.Ast.Ident_map.empty

let block_map : Tracelet.t Odefa_ast.Ast.Ident_map.t ref =
  ref Odefa_ast.Ast.Ident_map.empty

let graph_of_gate_tree tree =
  let g = G.create () in
  let counter = ref 0 in
  let add_node_edge prev this =
    match prev with
    | None -> G.add_vertex g (Either.first this)
    | Some prev -> G.add_edge g (Either.first prev) (Either.first this)
    (* G.add_edge_e g (G.E.create prev "" this) *)
  in
  let add_terminal node ending =
    match node with
    | None -> ()
    | Some node ->
        Int.incr counter;
        G.add_edge g (Either.first node)
          (Either.second (Fmt.str "%s %d" ending !counter))
  in
  let rec loop_tree ?(one_step = false) (prev : Gate.node option)
      (this : Gate.node) =
    let loop prev this = if one_step then () else loop_tree prev this in
    let add_node_rule ~key ~data =
      ignore @@ Hashtbl.add node_rule_set ~key ~data
    in
    match this.rule with
    | Pending ->
        (* add_terminal prev "pending .." *)
        add_node_rule ~key:this.key ~data:"Pending";
        add_node_edge prev this
    | Proxy _next ->
        add_node_rule ~key:this.key ~data:"Proxy";
        add_node_edge prev this (* loop_tree ~one_step:true (Some this) !next *)
    | Done _ ->
        add_node_rule ~key:this.key ~data:"Done";
        add_node_edge prev this
    | Mismatch ->
        add_node_rule ~key:this.key ~data:"Mismatch";
        add_node_edge prev this
    | Discard next ->
        add_node_rule ~key:this.key ~data:"Discard";
        add_node_edge prev this;
        loop (Some this) !next
    | Alias next ->
        add_node_rule ~key:this.key ~data:"Alias";
        add_node_edge prev this;
        loop (Some this) !next
    | To_first next ->
        add_node_rule ~key:this.key ~data:"To_first";
        add_node_edge prev this;
        loop (Some this) !next
    | Binop (n1, n2) ->
        add_node_rule ~key:this.key ~data:"Binop";
        add_node_edge prev this;
        List.iter ~f:(fun n -> loop (Some this) !n) [ n1; n2 ]
    | Cond_choice (n1, n2) ->
        add_node_rule ~key:this.key ~data:"Cond_choice";
        add_node_edge prev this;
        List.iter ~f:(fun n -> loop (Some this) !n) [ n1; n2 ]
    | Callsite (nf, nb, _) ->
        add_node_rule ~key:this.key ~data:"Callsite";
        add_node_edge prev this;
        List.iter ~f:(fun n -> loop (Some this) !n) (nf :: nb)
    | Condsite (nf, nb) ->
        add_node_rule ~key:this.key ~data:"Condsite";
        add_node_edge prev this;
        List.iter ~f:(fun n -> loop (Some this) !n) (nf :: nb)
    | Para_local (np, _) ->
        add_node_rule ~key:this.key ~data:"Para_local";
        add_node_edge prev this;
        List.iter
          ~f:(fun (n1, n2) ->
            loop (Some this) !n1;
            loop (Some this) !n2)
          np
    | Para_nonlocal (np, _) ->
        add_node_rule ~key:this.key ~data:"Para_nonlocal";
        add_node_edge prev this;
        List.iter ~f:(fun n -> loop (Some this) !n) np
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
      let x, _, _ = node.key in
      let clause =
        let c_id =
          match node.rule with
          | Para_local _ | Para_nonlocal _ -> node.block_id
          | _ -> x
        in
        Odefa_ast.Ast.Ident_map.Exceptionless.find (Id.to_ast_id c_id)
          !source_map
      in
      let content =
        Fmt.str "{%a | %a | %s}" Lookup_key.pp node.key
          (Fmt.option Odefa_ast.Ast_pp_graph.pp_clause)
          clause rule
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

  let edge_attributes e = [ `Label (E.label e) ]

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
