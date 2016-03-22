# The =benchmark.native= executable asserts that the benchmarks are behaving as
# expected by the analysis.
#
# This script times the execution and reports numbers.

TESTS = {
  "eta" => [1, 20],
  "sat" => [4, 5],
}

RULER = "==========================================="
CURRENT_DIRECTORY = File.expand_path("..", __FILE__)

require "benchmark"

TESTS.each_pair do |test_name, (k, maximum_copies)|
  (1..maximum_copies).each do |copies|
    file = "#{CURRENT_DIRECTORY}/../benchmark-sources/#{test_name}.code"

     command_line = "ruby #{CURRENT_DIRECTORY}/generate-big-example.rb odefa #{file} #{copies} | #{CURRENT_DIRECTORY}/../toploop.native --select-context-stack=#{k}ddpa --analyze-variables=all --disable-evaluation --disable-inconsistency-check --report-sizes"
    puts RULER
    puts "Test: `#{File.basename(file, ".*")}'."
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
end

puts RULER
