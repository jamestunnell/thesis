require 'open3'

PERIOD_WINDOWS = {
  7 => (36..78).step(3).to_a,
  14 => (24..54).step(3).to_a,
  30 => (12..36).step(3).to_a,
}

THESIS_DIR = "/home/james/projects/thesis/"
DATA_BASE_DIR = File.join(THESIS_DIR,"data")
OUT_BASE_DIR = File.join(THESIS_DIR,"runs")
EXE_PATH = File.join(THESIS_DIR,"src","model.R")

raise "Could not find base data directory: #{DATA_BASE_DIR}" unless Dir.exists?(DATA_BASE_DIR)
raise "Could not find base output directory: #{OUT_BASE_DIR}" unless Dir.exists?(OUT_BASE_DIR)

{
  "mongodb_coreserver_issues.txt" => { },
  "hibernate_orm_issues.txt" => { },
  "netbeans_java_issues.txt" => { :startdate => "2001-01-01" },
  "netbeans_platform_issues.txt" => { :startdate => "2001-01-01" },
}.each do |issues_file, opts|
  issues_path = File.join(DATA_BASE_DIR, issues_file)
  raise "Could not find issues file #{issues_path}" unless File.exists?(issues_path)

  print "Processing issues from #{issues_path}"

  PERIOD_WINDOWS.each do |period, windows|
    # out_path = File.join(OUT_BASE_DIR, "#{outdir}_#{period}")
    # if Dir.exists?(out_path)
    #   `rm -R #{out_path}`
    # end
    # Dir.mkdir(out_path)
    
    log_path = File.join(OUT_BASE_DIR, "log_#{issues_file}_#{period}.txt")
    options = "--periods=#{period} --windows=#{windows.join(",")} --ndiffs=0,1,2 --forcediff"

    if opts.has_key?(:outdir)
      out_path = File.join(OUT_BASE_DIR, "#{outdir}_#{period}")
      options += " --outdir=\"#{out_path}\""
    end

    if opts.has_key?(:startdate)
      options += " --startdate=#{opts[:startdate]}"
    end

    cmd_line = "#{EXE_PATH} #{issues_path} #{options}"
    File.open(log_path, "w") do |f|
      Open3.popen3(cmd_line) do |stdin, stdout, stderr, wait_thr|
        while line = stdout.gets
          print('.')
          f.write(line)
          f.flush
        end
      end      
    end

    puts ""
  end  
end