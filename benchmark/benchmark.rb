# The =benchmark.native= executable asserts that the benchmarks are behaving as
# expected by the analysis.
#
# This script times the execution and reports numbers.

TEST_ON_KS = (0..1)

RULER = "==========================================="
CURRENT_DIRECTORY = File.expand_path("..", __FILE__)

require "benchmark"

TEST_ON_KS.each do |k|
  Dir["#{CURRENT_DIRECTORY}/../benchmark-sources/*.code"].each do |file|
    command_line = "#{CURRENT_DIRECTORY}/../toploop.native --select-context-stack=#{k}ddpa --analyze-variables=all --disable-evaluation --disable-inconsistency-check < #{file}"
    puts RULER
    puts "Test: `#{File.basename(file, ".*")}'."
    puts "k: `#{k}'."
    puts "Command line: `#{command_line}'."
    puts

    duration = Benchmark.measure do
      system command_line#, {out: "/dev/null", err: "/dev/null"}
    end

    puts
    puts "Duration: `#{duration.to_s.strip}'."
  end
end
puts RULER
