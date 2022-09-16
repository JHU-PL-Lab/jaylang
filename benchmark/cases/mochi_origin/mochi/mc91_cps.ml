let rec m x (k: int -> unit): unit =
  if x > 100
  then k (x-10)
  else
    let f (r: int): unit = m r k in
    m (x+11) f

let main n =
  let k r = if n <= 101 then assert (r = 91) else () in
  m n k
