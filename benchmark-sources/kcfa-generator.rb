k = ARGV.first

k = k.to_i

if k <= 0
  abort <<-USAGE
Generate kCFA stress tests.

Usage:

  $ ruby #{__FILE__} <k>

Parameters:

  <k>: The `k' in `kCFA'. Must be positive.

Examples:

  $ ruby #{__FILE__} 1 | ./swan_toploop_main.native
USAGE
end

def kcfa k, i = 1
  if k < i
    xs = (1..k).map { |n| "x#{n}" }.join " "
    ys = (1..k).map { |n| "y#{n}" }.join " "
    "(fun z -> z #{xs}) (fun #{ys} -> y1)"
  else
    f = "f#{i}"
    a = "a#{i}"
    x = "x#{i}"
    "(fun #{f} -> let #{a} = #{f} true in #{f} false) (fun #{x} -> #{kcfa k, (i + 1)})"
  end
end

puts "#{kcfa k};;"
