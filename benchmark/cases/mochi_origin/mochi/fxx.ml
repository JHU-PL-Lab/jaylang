let f (x: int) (y: int) = assert (not (x>0 && y<=0))
let main (x: int) = f x x
