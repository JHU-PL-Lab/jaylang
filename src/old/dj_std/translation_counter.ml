let global_counter = ref 0

let fresh_n () =
  let c = !global_counter in
  global_counter := c + 1 ;
  c
