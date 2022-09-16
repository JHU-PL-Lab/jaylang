let g (x:int) (y:unit) = x
let twice f (x:unit->int) (y:unit) = f (f x) y
let neg x (y:unit) = - x ()
let main n =
  if n>=0 then
    let z = twice neg (g n) () in
    assert (z>=0)
  else ()
