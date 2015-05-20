# -*- coding: utf-8 -*-
"""Extract occurances of issues (bugs, new features, etc.) from JIRA issue 
tracking report file(s), and place them into a text-table format.

For each issue, the following information will appear in the table: type,
priority, and days to resolve.

Usage:
    extract_jira.py mongodb INPUT_GLOB OUTPUT_FILE [options]
    extract_jira.py hibernate INPUT_GLOB OUTPUT_FILE [options]

Arguments:
    INPUT_GLOB A glob string, used to find input file(s)
    OUTPUT_FILE The text file to store the table to
    
Options:
"""

MONGODB_TYPES = {
    1: "bug", 2: "newfeature", 3: "task", 4: "improvement",
    5: "subtask", 6: "question", 13: "backport"
}

HIBERNATE_TYPES = {
    1: "bug", 2: "newfeature", 3: "task", 4: "improvement",
    5: "improvement", 6: "question", 7: "subtask", 8: "removefeature", 
    10:"story", 11: "techtask", 13: "backport"
}

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

if(args["mongodb"]):
    print("Extracting from MongoDB JIRA project")
    type_map = MONGODB_TYPES
    
if(args["hibernate"]):
    print("Extracting from Hibernate JIRA project")
    type_map = HIBERNATE_TYPES

type_subtask = type_map.keys()[type_map.values().index("subtask")]
print("sub-task type is %d" % type_subtask)

issues = []
for i in range(n):
    in_fname = in_fnames[i]
    sys.stdout.write("%03d / %03d | %s" % (i+1,n,in_fname))
    loaded = load_issues(in_fname, type_map)
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
    elif issue.type == type_subtask:
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
