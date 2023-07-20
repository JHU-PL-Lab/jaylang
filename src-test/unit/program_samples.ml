let parse = Jayil_parser.Parse.parse_string
let e1 = parse "\nt = 1\n"
let e2 = parse "\na = 0;\nb = 1;\nt = a + b;\n"
let e3 = parse "\na = 0;\nb = 1;\nc = a < b;\nt = c ? (r1 = 1) : (r2 = 0);\n"
let e4 = parse "\na = 0;\nb = 1;\nc = {f1 = a, f2 = b};\nt = c.f2\n"
