CASES = [
  # BASELINE
  # {ack: {k: 0, repetitions: 100}},
  # {alex: {k: 0, repetitions: 100}},
  # {blur: {k: 0, repetitions: 100}},
  # {cpstak: {k: 0, repetitions: 100}},
  {eta: {k: 0, repetitions: 100}},
  {kcfa2: {k: 0, repetitions: 100}},
  {kcfa3: {k: 0, repetitions: 100}},
  # {loop2: {k: 0, repetitions: 100}},
  # {mj09: {k: 0, repetitions: 100}},
  # {recursion: {k: 0, repetitions: 100}},
  {sat: {k: 0, repetitions: 100}},
  # {state: {k: 0, repetitions: 100}},
  # {tak: {k: 0, repetitions: 100}},

  # HIGHER K
  # {eta: {k: 1, repetitions: 10}},
  # {kcfa2: {k: 5, repetitions: 10}},
  # {kcfa3: {k: 7, repetitions: 10}},
  # {sat: {k: 4, repetitions: 10}},

  # BASELINE (continuation)
  {church: {k: 0, repetitions: 1}},
  {mbrotZ: {k: 0, repetitions: 1}},

  # HIGHER K (continuatiackon)
  # {church: {k: 1, repetitions: 1}},
  # {mbrotZ: {k: 1, repetitions: 1}},
]

RULER = "==========================================="
CURRENT_DIRECTORY = File.expand_path("..", __FILE__)

require "benchmark"

CASES.each do |the_case_hash|
  the_case_hash.each_pair do |(the_case_name, the_case)|
    k = the_case.fetch(:k)
    repetitions = the_case.fetch(:repetitions)
    file = "#{CURRENT_DIRECTORY}/../benchmark-sources/#{the_case_name}.swan"
    command_line = "#{CURRENT_DIRECTORY}/../swan_toploop_main.native --select-context-stack=#{k}ddpa --analyze-variables=all --disable-evaluation --disable-inconsistency-check --report-sizes < #{file}"
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
