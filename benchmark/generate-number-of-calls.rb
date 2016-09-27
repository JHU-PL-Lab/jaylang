times, = ARGV

if times.nil?
  abort <<-USAGE
Usage: ruby #{__FILE__} <times>
USAGE
end

times = times.to_i

puts <<-CODE
let identity identityX = identityX in
let x = 3 in
CODE

times.times do
  print "identity x;"
end
puts ";"
