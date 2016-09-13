COPIES = 1..10
K = 1

RULER = "==========================================="
CURRENT_DIRECTORY = File.expand_path("..", __FILE__)

require "benchmark"

COPIES.each do |copies|
  k = K

  command_line = "ruby #{CURRENT_DIRECTORY}/generate-recursive-program.rb #{copies} | #{CURRENT_DIRECTORY}/../swan_toploop_main.native --select-context-stack=#{k}ddpa --analyze-variables=all --disable-evaluation --disable-inconsistency-check --report-sizes"
  puts RULER
  puts "k: `#{k}'."
  puts "Copies: `#{copies}'."
  puts "Command line: `#{command_line}'."
  puts

  duration = Benchmark.measure do
    system command_line
  end

  puts
  puts "Duration: `#{duration.to_s.strip}'."
end

puts RULER
