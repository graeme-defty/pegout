require 'rake/clean'

# Path on the local filesystem to install pegout/ire scripts to
BIN_INSTALL_DIR = "/usr/local/bin"

task :default => %w(check_erl_version build test)

# Returns the installed Erlang version
def erlang_version
  version = `erl -version 2>&1`.strip.match(/\d\.\d(\.\d)?$/)
  unless version
   STDERR.puts "Error retrieving Erlang version.  Do you have it installed?" 
   exit 1
  end
  
  version[0]
end

# Evaluate the given Erlang statement
def erl_eval(cmd, *pa)
  pa_str = pa.empty? ? "" : "-pa #{pa.join(' ')}"
  sh "erl -noshell #{pa_str} -eval '#{cmd}' -s erlang halt"
end

# Ensure the version of Erlang installed is recent enough
task :check_erl_version do
  print "Checking Erlang version... "
  version = erlang_version
  
  if version >= "5.6.3"
    puts "#{version} (ok)"
  else
    puts "#{version} (too old)"
    puts "Sorry, the version of Erlang you have installed is too old to run pegout"
    puts "pegout requires a minimum Erlang version of R12B-3 (5.6.3)"
    puts "Please see http://github./pegout.org/wiki/Building#Prerequisites"
    exit 1
  end
end

## Neotoma (PEG for Erlang)
#NEOTOMA_FILES = %w(neotoma neotoma_parse neotoma_peg)
#task :neotoma => NEOTOMA_FILES.map { |f| "src/neotoma/ebin/#{f}.beam" }
#
#NEOTOMA_FILES.each do |f|
#  input = "src/neotoma/src/#{f}.erl"
#  file "src/neotoma/ebin/#{f}.beam" => input do
#    sh "erlc -o src/neotoma/ebin #{input}"
#  end
#end

# Parser
file "src/pegout_parse.erl" => "src/pegout_parse.peg" do
  erl_eval 'neotoma:file("src/pegout_parse.peg")'
end

# Generate an output path for the given input file
def output_file(input_file, dir = 'ebin/', ext = '.beam')
  dir + File.basename(input_file).sub(/\.\w+$/, ext)
end

GENERATED_SRC = %w(src/pegout_parse.erl)
ERL_SRC = (GENERATED_SRC + FileList.new('src/*.erl')).uniq
ERL_DEST = ERL_SRC.map { |input| output_file(input) }

QUIET_SRC = %w(src/pegout_parse.erl)

ERL_SRC.each do |input|
  file output_file(input) => input do
    sh "erlc +debug_info -o ebin #{input}"
  end
end


# Build rules
task :build   => ERL_DEST

# Test suite
task :test => :build do
  erl_eval 'eunit:test(parser_test)', 'ebin'
  sh "bin/pegout test/runner.re"
end

## Benchmarks
#BENCHMARK_SRC = FileList.new('benchmarks/**/*.erl')
#BENCHMARK_DEST = BENCHMARK_SRC.map { |input| output_file(input, 'benchmarks/ebin/') }
#
#BENCHMARK_SRC.each do |input|
#  file output_file(input, 'benchmarks/ebin/') => input do
#    sh "erlc +debug_info -o benchmarks/ebin #{input}"
#  end
#end
#
#task :benchmark => BENCHMARK_DEST do
#  sh "bin/pegout benchmarks/runner.re"
#end

# Cleaning
CLEAN.include %w(ebin/* src/pegout_parse.erl)
CLEAN.include %w(**/*.beam)
CLEAN.include "erl_crash.dump"

#
# Installing
#

# Retrieve the directory Erlang libraries are stored in
def erl_lib_dir
  $erl_lib_dir ||= `erl -noshell -eval "io:format(code:lib_dir())" -s erlang halt`
end


# Directory to install pegout into
def pegout_install_dir
  File.join(erl_lib_dir, 'pegout', '')
end

## Munge pegout launcher scripts before installing
#def munge_script(src, dest)
#  str = File.read(src)
#  
#  # Remove pegout_HOME declaration
#  str.gsub!(/^export pegout_HOME=.*$/, '')
#  
#  # Remove EXTRA_PATHS declaration
#  str.gsub!(/^EXTRA_PATHS=.*$/, '')
#  
#  # Remove $EXTRA_PATHS variables
#  str.gsub!(/\$EXTRA_PATHS/, '')
#  
#  # Strip all the extraneous newlines
#  str.gsub!(/\n\n+/m, "\n\n")
#  
#  File.open(dest, "w", 0755) { |file| file << str }
#end

directory BIN_INSTALL_DIR

task :install => [:check_erl_version, :build, BIN_INSTALL_DIR] do
  pegout_dir = pegout_install_dir
  STDERR.puts "*** Installing pegout into: #{pegout_dir}"
  
  rm_r pegout_dir if File.exist?(pegout_dir)
  mkdir pegout_dir
  cp_r "ebin", pegout_dir
  
  %w[ire pegout pegoutc].each do |script|
    src = File.expand_path("../bin/#{script}", __FILE__)
    dst = "#{BIN_INSTALL_DIR}/#{script}"
    
    STDERR.puts "Creating #{dst}"
    munge_script src, dst
  end
end

task :uninstall do
  pegout_dir = pegout_install_dir

  rm_r pegout_dir if File.directory?(pegout_dir)
  %w[ire pegout pegoutc].each { |script| rm_f "#{BIN_INSTALL_DIR}/#{script}" }
end
