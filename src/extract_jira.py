# -*- coding: utf-8 -*-
"""Extract occurances of issues (bugs, new features, etc.) from JIRA issue 
tracking report file(s), and place them into a text-table format.

For each issue, the following information will appear in the table: type,
priority, and days to resolve.

Usage:
    extract_jira.py INPUT_GLOB OUTPUT_FILE [options]

Arguments:
    INPUT_GLOB A glob string, used to find input file(s)
    OUTPUT_FILE The text file to store the table to
    
Options:
"""

from docopt import docopt
args = docopt(__doc__)
print("Running extract with args %s" % args)

from jira_xml import *
from glob import glob
import sys

in_fnames = sorted(glob(args["INPUT_GLOB"]) )
n = len(in_fnames)
out_fname = args["OUTPUT_FILE"]

issues_by_key = {}
issues_by_type = {}

issues = []
for i in range(n):
    in_fname = in_fnames[i]
    sys.stdout.write("%03d / %03d | %s" % (i+1,n,in_fname))
    loaded = load_issues(in_fname)
    sys.stdout.write(" -> found %d issues\n" % len(loaded))
    issues += loaded
issues = dict([[i.key,i] for i in issues])

# for tasks with sub-tasks: if it can be found, covert it to same type as parent
converted = {}
for issue in issues.values():
    for subtask_key in issue.subtasks:
        if subtask_key in issues:
            if issue.type in converted:
                converted[issue.type] += 1
            else:
                converted[issue.type] = 1
            issues[subtask_key].type = issue.type
print("Conversions: %s" % converted)

# remaining subtasks are tossed out
nunfixed = 0
nsubtasks = 0
for key, issue in issues.items():
    if not issue.isfixed():
        issues.pop(key)
        nunfixed += 1
    elif issue.type == TYPE_SUBTASK:
        issues.pop(key)
        nsubtasks += 1
print("Tossed out %d unfixed" % nunfixed)
print("Tossed out %d orphaned sub-tasks" % nsubtasks)

header = ["type","priority","created","resolved","fixversion"]
rows = []
for i in issues.values():
    rows.append((i.type_str(), i.priority, i.created, i.resolved, i.fixversion))

import csv
with file(out_fname, 'wb') as csvfile:
    writer = csv.writer(csvfile, delimiter=' ', quotechar='"', 
                        quoting=csv.QUOTE_MINIMAL)
    writer.writerow(header)
    writer.writerows(rows)
