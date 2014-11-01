# -*- coding: utf-8 -*-
"""Extract occurances of issues (bugs, new features, etc.) from JIRA issue 
tracking report file(s), and place them into a text-table format.

For each issue, the following information will appear in the table: type,
priority, and days to resolve.

Usage:
    extract.py INPUT_GLOB OUTPUT_FILE

Arguments:
    INPUT_GLOB A glob string, used to find input file(s)
    OUTPUT_FILE The text file to store the table to
"""

from docopt import docopt
args = docopt(__doc__)
print("Running extract with args %s" % args)

SEC_IN_DAY = float(24*60*60)
def days_to_resolve(created, resolved):
    time_delta = resolved - created
    return time_delta.total_seconds() / SEC_IN_DAY


from jira_xml import *
from glob import glob

in_fnames = sorted(glob(args["INPUT_GLOB"]) )
n = len(in_fnames)
out_fname = args["OUTPUT_FILE"]

header = ["type","priority","daystoresolve"]
rows = []
for i in range(n):
    in_fname = in_fnames[i]
    print("%03d / %03d | %s" % (i+1,n,in_fname))
    
    items = keep_closed(keep_fixed(load_items(in_fname)))
    frows = []
    for item in items:
        row = []
        
        row.append(read_type(item))
        row.append(read_priority(item))
        
        created, resolved = read_created(item), read_resolved(item)
        days = days_to_resolve(created,resolved)
        row.append(days)
        
        frows.append(row)
        #[ [read_typeid(item), read_priorityid(item), read_statusid(item), read_resolutionid(item), read_fixversion(item), read_created(item), read_resolved(item)] for item in items ]
    rows += frows

import csv
with file(out_fname, 'wb') as csvfile:
    writer = csv.writer(csvfile, delimiter=' ', quotechar='"', 
                        quoting=csv.QUOTE_MINIMAL)
    writer.writerow(header)
    writer.writerows(rows)
