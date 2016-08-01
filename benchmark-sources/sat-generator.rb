k = ARGV.first

k = k.to_i

if k <= 0
  abort <<-USAGE
Generate SAT solver stress tests.

Usage:

  $ ruby #{__FILE__} <arity>

Parameters:

  <arity>: The arity---number of parameters---required by the proposition.

Standard input:

  The definition of the proposition.

Examples:

  $ cat benchmark-sources/sat-proposition.txt | ruby #{__FILE__} 4 | ./swan_toploop_main.native
USAGE
end

def sat k, i = 1
  if k < i
    ns = (1..k).map { |n| "n#{n}" }.join " "
    "p #{ns}"
  else
    n = "n#{i}"
    "try (fun #{n} -> #{sat k, (i + 1)})"
  end
end

xs = (1..k).map { |n| "x#{n}" }.join " "
puts <<-SAT
let phi #{xs} =
  #{STDIN.read}
in
let try f =
  (f true) or (f false)
in
let satsolve#{k} p =
  #{sat k}
in
satsolve#{k} phi;;
SAT
