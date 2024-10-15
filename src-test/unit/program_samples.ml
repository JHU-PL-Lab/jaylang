let parse = Jayil_parser.Parse.parse_string
let e1 = parse "t = 1"
let e2 = parse "a = 0; b = 1; t = a + b"
let e3 = parse "a = 0; b = 1; c = a < b; t = c ? (r1 = 1) : (r2 = 0)"
let e4 = parse "a = 0; b = 1; c = {f1 = a, f2 = b}; t = c.f2"
