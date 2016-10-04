# The =benchmark.native= executable asserts that the benchmarks are behaving as
# expected by the analysis.
#
# This script times the execution and reports numbers.

TESTS = {
  "tak" => [5, 1],
}

RULER = "==========================================="
CURRENT_DIRECTORY = File.expand_path("..", __FILE__)

require "benchmark"

TESTS.each_pair do |test_name, (maximum_k, repetitions)|
  (1..maximum_k).each do |k|
    file = "#{CURRENT_DIRECTORY}/../benchmark-sources/#{test_name}.code"

    command_line = "#{CURRENT_DIRECTORY}/../core_toploop_main.native --select-context-stack=#{k}ddpa --analyze-variables=all --disable-evaluation --disable-inconsistency-check --report-sizes < #{file}"
    puts RULER
    puts "Test: `#{File.basename(file, ".*")}'."
    puts "k: `#{k}'."
    puts "Repetitions: `#{repetitions}'."
    puts "Command line: `#{command_line}'."
    puts

    duration = Benchmark.measure do
      repetitions.times do |repetition|
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
