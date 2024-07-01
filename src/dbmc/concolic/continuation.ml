
type ('b, 'a) t = { continuation : ('a -> 'b) -> 'b}

let bind (ar_r : ('r, 'a) t) : ('a -> ('r, 'b) t) -> ('r, 'b) t =
  fun a_br_r ->
    { continuation =
      fun br ->
        match ar_r with
        | { continuation = ar_r } ->
          ar_r (
            fun a -> 
              match a_br_r a with
              | { continuation = br_r } ->
                br_r br
          )
    }
      