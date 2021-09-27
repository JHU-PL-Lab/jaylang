open Core

let enum_unary vals op = List.map vals ~f:(fun v -> (v, op v))

let enum_binop vals op =
  List.cartesian_product vals vals
  |> List.map ~f:(fun (v1, v2) -> ((v1, v2), op v1 v2))

module Bool_fact = struct
  let vals = [ true; false ]

  let not_facts = enum_unary vals not

  let and_facts = enum_binop vals ( && )

  let or_facts = enum_binop vals ( || )
end

module Int_fact = struct
  let vals = [ 0; 1; 2 ]

  let add_facts = enum_binop vals ( + )
end
