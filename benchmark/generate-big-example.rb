language, file, times = ARGV

if language.nil? || ! ["odefa", "scheme"].include?(language) || file.nil? || times.nil?
  abort <<-USAGE
Usage: ruby #{__FILE__} <odefa|scheme> <file> <times>
USAGE
end

case language
when "odefa"
  KEYWORDS = %w[
    fun
    ref
    or
    and
    not
    false
    true
  ]
  COMMENT = "#"
when "scheme"
  KEYWORDS = %w[
    define
    cond
    else
    letrec
    lambda
    if
    let
    #t
    #f
    not
    and
    or
  ]
  COMMENT = ";"
end
times = times.to_i

benchmark = File.read(file).gsub(/#{COMMENT}[^\n]*\n/, "")

result = ""

times.times do |i|
  result += benchmark.gsub(/#?[a-zA-Z][a-zA-Z0-9]*/) do |word|
    if KEYWORDS.include? word
      word
    else
      "#{word}distinctname#{i}"
    end
  end
end

puts result
