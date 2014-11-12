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

from jira_xml import *
from glob import glob

in_fnames = sorted(glob(args["INPUT_GLOB"]) )
n = len(in_fnames)
out_fname = args["OUTPUT_FILE"]

issues_by_key = {}
issues_by_type = {}

issues = []
for i in range(n):
    in_fname = in_fnames[i]
    print("%03d / %03d | %s" % (i+1,n,in_fname))
    issues += load_issues(in_fname)
issues = dict([[i.key,i] for i in issues])

# for each sub-task, either: convert it plain issue of same type as parent, or 
# toss it if parent can't be found

# for tasks with sub-tasks: if it can be found, covert it to same type as parent
for issue in issues.values():
    for subtask_key in issue.subtasks:
        if subtask_key in issues:
            issues[subtask_key] = issues[subtask_key].reissue(issue.type)

# remaining subtasks are tossed out
for key, issue in issues.items():
    if issue.type == TYPE_SUBTASK:
        issues.pop(key)

header = ["type","priority","daystoresolve"]
rows = [ (i.type_str(), i.priority, i.days_to_resolve()) for i in issues.values() ]

import csv
with file(out_fname, 'wb') as csvfile:
    writer = csv.writer(csvfile, delimiter=' ', quotechar='"', 
                        quoting=csv.QUOTE_MINIMAL)
    writer.writerow(header)
    writer.writerows(rows)
