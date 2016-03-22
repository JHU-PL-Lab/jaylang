# The =benchmark.native= executable asserts that the benchmarks are behaving as
# expected by the analysis.
#
# This script times the execution and reports numbers.

REPETITIONS = 100
TEST_ON_KS = [0]

RULER = "==========================================="
CURRENT_DIRECTORY = File.expand_path("..", __FILE__)

require "benchmark"

TEST_ON_KS.each do |k|
  Dir["#{CURRENT_DIRECTORY}/../benchmark-sources/*.code"].each do |file|
    command_line = "#{CURRENT_DIRECTORY}/../toploop.native --select-context-stack=#{k}ddpa --analyze-variables=all --disable-evaluation --disable-inconsistency-check --report-sizes < #{file}"
    puts RULER
    puts "Test: `#{File.basename(file, ".*")}'."
    puts "k: `#{k}'."
    puts "Repetitions: `#{REPETITIONS}'."
    puts "Command line: `#{command_line}'."
    puts

    duration = Benchmark.measure do
      REPETITIONS.times do |repetition|
        if repetition % 10 == 0
          puts "Repetition: #{repetition}."
        end
        if repetition == 0
          system command_line
        else
          system command_line, {out: "/dev/null", err: "/dev/null"}
        end
      end
    end

    puts
    puts "Duration: `#{duration.to_s.strip}'."
  end
end
puts RULER
