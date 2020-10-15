(* open Batteries *)
open Utils

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
p = 1;
f = fun t -> (
  rf = 1
);
g = f;
a = g p;
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
p = z2 u;
p2 = p;
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

let e10_2 = parse "
a = input;
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

let e18 = parse "
one = 1;
f = fun s -> (
    f0 = fun a -> (
        b = a <= one;
        r = b ? (
                r1 = a
            ) : (
                ss = s s;
                v = a - one;
                r2 = ss v;
            );
        n1 = r;
    );
);
ff = f f;
x3 = 3;
z = ff x3;
"

let e19 = parse "
one = 1;
two = 2;
f = fun fx -> (
  f0 = fun fy -> (
    f1 = fx
  )
);
g = fun gx -> (
  g0 = fun gy -> (
    g1 = gx
  )
);
app = fun pf -> (
  rapp = fun px -> (
    r = pf px;
    target = 1;
    rr = r;
  )
);
tf1 = app f;
tf = tf1 one;
tg2 = app g;
tg = tg2 two;
"

let e19_3 = parse "
one = 1;
two = 2;
three = 3;
f = fun fx -> (
  f0 = fun fy -> (
    f1 = fx
  )
);
app = fun pf -> (
  rapp = fun px -> (
    r = pf px;
    target = 1;
    rr = r;
  )
);
af1 = app f;
tf1 = af1 one;
af2 = app f;
tf2 = af2 two;
af3 = app f;
tf3 = af3 three;
"

let e19_2 = parse "
one = 1;
two = 2;
three = 3;
f = fun fx -> (
  f0 = fun fy -> (
    f1 = fx
  )
);
g = fun gx -> (
  g0 = fun gy -> (
    g1 = gx
  )
);
h = fun hx -> (
  h0 = fun hy -> (
    h1 = hx
  )
);
app = fun pf -> (
  rapp = fun px -> (
    r = pf px;
    target = 1;
    rr = r;
  )
);
tf1 = app f;
tf = tf1 one;
tg1 = app g;
tg = tg1 two;
th1 = app g;
th = th1 three;
"

let e20 = parse "
n1 = 1;
n2 = 2;
n3 = 3;
f1 = fun x1 -> (
  rf1 = fun y1 -> (
    rrf1 = x1
  )
);
f2 = fun x2 -> (
  rf2 = fun y2 -> (
    rrf2 = x2
  )
);
f3 = fun x3 -> (
  rf3 = fun y3 -> (
    rrf3 = x3
  )
);

ap = fun f -> (
  n0 = 0;
  r = f n0;
  target = r
);

tf1 = f1 n1;
tf2 = f2 n2;
tf3 = f3 n3;

g1 = ap tf1;
g2 = ap tf2;
g3 = ap tf3
"

let e21 = parse "
one = 1;
single = one;
two = one + one;
f = fun x -> (
  tf = 1;
  rf = tf;
);
t = f one;
target = 1;
"

let e22 = parse "
one = 1;
two = 2;
three = 3;
four = 4;
f = fun x -> (
  n = x;
  g = fun y -> (
    m = x;
    target = one;
  );
);
a1 = f one;
a2 = f two;
b1 = a1 three;
b2 = a2 four;
"