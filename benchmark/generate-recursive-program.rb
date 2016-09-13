times, = ARGV

if times.nil?
  abort <<-USAGE
Usage: ruby #{__FILE__} <times>
USAGE
end

times = times.to_i

puts <<-CODE
let multiply (multiplyLeft, multiplyRight) =
  if multiplyRight < 0 then
    0 - multiply (multiplyLeft, 0 - multiplyRight)
  else if multiplyRight == 0 then
    0
  else if multiplyRight == 1 then
    multiplyLeft
  else
    multiplyLeft + multiply (multiplyLeft, multiplyRight - 1)
  end end end
in
CODE

times.times do
  print "multiply (2, 3);"
end
puts ";"
