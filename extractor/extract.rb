# frozen_string_literal: true

require 'asciidoctor'
require 'json'
require 'optparse'
require 'tempfile'
require 'fileutils'

parser = OptionParser.new do |opts|
  opts.banner = <<USAGE
  Usage: extract.rb attributes path/to/asciidoc
                  | set-attr path/to/asciidoc attr value
USAGE
end
parser.parse!

if ARGV.empty?
  puts parser.banner
  exit!
end

def attributes(path)
  document = Asciidoctor.load_file path
  puts JSON.dump(document.attributes)
rescue Errno::ENOENT
  puts '{}'
  exit
end

def set_attr(path, attr, value)
  temp = Tempfile.new('adoc')

  # Copy document to temp using doing the substitution
  document = File.new(path, 'r')
  document.rewind
  before_header = true
  after_header = false
  in_attr = false
  found = false
  document.each do |line|
    if after_header
      temp.puts line
    elsif in_attr
      in_attr = false if line !~ /\\\s*$/
    elsif before_header
      before_header = false if line =~ /^= /
      temp.puts line
    elsif line =~ /^== / || line =~ /^\s*$/
      after_header = true
      temp.puts ":#{attr}: #{value}" unless found
      temp.puts line
    else
      m = line.match(/^\s*:([a-zA-Z0-9_-]+):/)
      if m && m[1] == attr
        in_attr = true if line =~ /\\\s*$/
        found = true
        temp.puts ":#{attr}: #{value}"
      else
        temp.puts line
      end
    end
  end
  # Necessary for the case the file end after the attributes
  temp.puts ":#{attr}: #{value}" unless found
  document.close
  temp.close

  # Move temp to document
  File.delete path
  FileUtils.cp temp.path, path
  temp.unlink
end

case ARGV[0]
when 'attributes'
  if ARGV.length != 2
    puts parser.banner
    exit!
  end
  attributes ARGV[1]
when 'set-attr'
  if ARGV.length != 4
    puts parser.banner
    exit!
  end
  set_attr ARGV[1], ARGV[2], ARGV[3]
else
  puts "Unknown command ", ARGV[0]
end
