#!/usr/bin/env ruby

exe_name = File.basename(__FILE__)

doc = <<DOCOPT
Extract issue data from Bugzilla MySQL database.

Usage:
  #{exe_name} DBNAME USER PASS [options]
  #{exe_name} -h | --help
  #{exe_name} --version

Arguments:
  DBNAME The MySQL database name
  USER   MySQL username to access the DB
  PASS   MySQL password to access the DB
  
Options:
  --dbhost=H    The host location for the MySQL DB [default: localhost]
  --outdir=O    Directory to place output files [default: .]
  -h --help     Show this screen.
  --version     Show version.

DOCOPT

require 'docopt'
begin
  args = Docopt::docopt(doc)
  puts args
rescue Docopt::Exit => e
  puts e.message
  exit
end

DBHOST = args["--dbhost"]
DBNAME = args["DBNAME"]
USER = args["USER"]
PASS = args["PASS"]
OUTDIR = args["--outdir"]

unless Dir.exist?(OUTDIR)
  puts "Output directory #{OTUDIR} does not exist"
  exit
end

require 'sequel'
DB = Sequel.connect("do:mysql://#{USER}:#{PASS}@#{DBHOST}/#{DBNAME}")

fixings = DB[:bugs_activity].to_a.select {|activity| activity[:added] == "FIXED" }

fixings_by_bug = Hash[
  fixings.map {|activity| [activity[:bug_id], activity]}
]

bugs_by_prod = {}
DB[:bugs].each do |bug|
  prod_id = bug[:product_id]
  if bugs_by_prod.has_key? prod_id
    bugs_by_prod[prod_id].push bug
  else
    bugs_by_prod[prod_id] = [bug]
  end
end

HEADER = ["type","priority","created","resolved","fixversion","product","component"]

require 'csv'

bugs_by_prod.each do |prod_id,bugs|
  puts "Writing table for #{bugs.size} bugs that have product id #{prod_id}"
  fname = File.join(OUTDIR, "prod_#{prod_id}_#{bugs.size}_bugs.txt")
  CSV.open(fname, 'wb', :headers => HEADER, :col_sep => " ", :write_headers => true) do |csv|
    nunfixed = nfixed = fixtimes_notfound = 0

    bugs.each do |bug|
      fixing = fixings_by_bug[bug[:bug_id]]

      if bug[:resolution] == "FIXED" && fixing
        nfixed += 1

        type = case bug[:cf_bug_type]
        when "DEFECT" then "bug"
        when "ENHANCEMENT" then "improvement"
        when "TASK" then "newfeature"
        else "NA"
        end

        pri = bug[:priority][1]
        created = bug[:creation_ts].to_s
        ver = bug[:version]
        resolved = fixing[:bug_when].to_s
        prod = bug[:product_id]
        component = bug[:component_id]

        csv << [type,pri,created,resolved,ver,prod,component]
      else
        #puts "Could not find fixing for bug #{bug[:bug_id]}"
        nunfixed += 1
      end
    end

    ntotal = nfixed + nunfixed
    pct_unfixed = (100 * nunfixed / ntotal.to_f).round
    puts "Found #{nfixed} fixed issues"
    puts "Passed over #{nunfixed} (#{pct_unfixed}%) unfixed issues"
    puts ""
  end
end
