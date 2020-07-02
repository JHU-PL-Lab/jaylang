open Batteries
open Odefa_parser

let parse s = 
  s
  |> IO.input_string
  |> Parser.parse_program

let e1 = parse "
b = fun t -> (
  a = 1
);
t = 1
"

let e2 = parse "
b1 = fun t1 -> (
  z = 1;
  b2 = fun t2 -> (
    a = 1
  );
  b = true;
  t = b2 z
)
"

let e3 = parse "
wb = true;
w2 = wb ? (
  w1 = fun d1 -> (
    c = true;
    b = c ? (
      z = 1;
      x = 1
    ) : (
      y = 2
    )
  )
) : (
  t = 1
)
"

let e4 = parse "
c = true;
b = c ? (
  z = 1;
  x = 1
) : (
  b2 = c ? (
    z2 = 1;
    x2 = 1;
  ) : (
    y2 = 2;
    id = fun x2 -> (
      r = x2
    );
    u2 = id y2
  )
)
"

let e5 = parse "
c = true;
id = fun x -> (
  rid = x
);
f = fun a -> (
  g = fun b -> (
    h = a c
  )
);
b1 = f id;
b2 = b1 id
"

let e6 = parse "
t = 1;
f = fun t -> (
  rf = 1
);
g = f;
a = g t;
x = 1;
"

let e7 = parse "
u = 1;
z = fun w -> (
  f = fun t -> (
    rf = 1
  );
  g = f;
  a = g u;
  x = 1;
);
z2 = z;
p = z2 u
"

let e8 = parse "
a1 = 1;
a2 = 2;
f = fun t -> (
  x = 1;
  rf = t
);
c1 = f a1;
c2 = f a2
"

let e9 = parse "
a1 = 1;
a2 = 2;
a3 = 3;
f = fun t -> (
  x = 1;
  rf = t
);
g = f;
c1 = f a1;
c2 = f a2;
c3 = f a3
"

let e10 = parse "
a = true;
b = a ? (r1 = 1) : (r2 = 1);
e = 1;
c = b;
target = 1
"

let e11 = parse "
za = true;
b = za ? (r1 = 1; target = 1) : (r2 = 1);
e = 1;
c = b
"

let e11_2 = parse "
za = true;
b = za ? (r1 = 1) : (r2 = 1; target = 1);
e = 1;
c = b
"

let e12 = parse "
x = input;
y = 1;
za = x == y;
b = za ? (r1 = 1) : (r2 = 1; target = 1);
e = 1;
c = b
"

let e13 = parse "
x = input;
y = 1;
za = x == y;
b = za ? (r1 = 1) : (r2 = 1);
e = 1;
c = b;
target = 1
"

let e14 = parse "
f = fun x -> ( r = x x );
t = f f;
target = 1
"

let e15 = parse "
t = true;
f = false;
g = fun c -> (
  r = c ? (r1 = 1) : (r2 = 2);
  rr = r
);
p = g t;
target = p
"

let e16 = parse "
t = true;
f = false;
g = fun c -> (
  r = c ? (r1 = 1) : (r2 = 2);
  rr = r
);
p = g f;
target = p
"

let e17 = parse "
t = true;
f = false;
g = fun c -> (
  r = c ? (r1 = 1) : (r2 = 2);
  rr = r
);
p = g t;
pp = g f;
target = p
"