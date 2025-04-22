
let count : (int ref ) = ref 0

let decrement (x : int ref) = x := (!x) - 1

let increment (x : int ref) = x := (!x) + 1