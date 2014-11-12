# -*- coding: utf-8 -*-
from BeautifulSoup import BeautifulSoup
import time 
import datetime as dt

TYPE_BUG = 1
TYPE_NEWFEATURE = 2
TYPE_TASK = 3
TYPE_IMPROVEMENT = 4
TYPE_SUBTASK = 5
TYPE_QUESTION = 6

TYPES = { TYPE_BUG: "bug", TYPE_NEWFEATURE: "newfeature",
           TYPE_TASK: "task", TYPE_IMPROVEMENT: "improvement",
           TYPE_SUBTASK: "subtask", TYPE_QUESTION: "question" }

STATUS_CLOSED = 6
RESOLUTION_FIXED = 1

SEC_IN_DAY = float(24*60*60)
class Issue:
    def __init__(self, key, typ, pri, created, resolved, subtasks = []):
        self.key = key
        self.type = typ
        self.priority = pri
        self.created  = created
        self.resolved  = resolved
        self.subtasks = subtasks

    def days_to_resolve(self):
        time_delta = self.resolved - self.created
        return time_delta.total_seconds() / SEC_IN_DAY

    def type_str(self):
        return TYPES[self.type]
        
    def reissue(self, newtype):
        return Issue(self.key, newtype, self.priority, self.created, self.resolved)

def load_issues(xml_fname):
    f = file(xml_fname)
    bs = BeautifulSoup(f.read())
    items = keep_closed(keep_fixed(bs.channel.findAll("item")))
    return [ Issue(read_key(i),read_type(i),read_priority(i),read_created(i),
                   read_resolved(i),read_subtasks(i)) for i in items ]

def isfixed(item):
    return int(item.resolution['id']) == RESOLUTION_FIXED

def isclosed(item):
    return int(item.status['id']) == STATUS_CLOSED

def keep_fixed(items):
    return filter(lambda x: isfixed(x), items)

def keep_closed(items):
    return filter(lambda x: isclosed(x), items)

def read_key(item):
    return int(item.key['id'])

def read_type(item):
    return int(item.type['id'])

def read_priority(item):
    return int(item.priority['id'])

def read_subtasks(item):
    return [ int(i['id']) for i in item.subtasks.findAll("subtask") ]

def read_fixversion(item):
    return str(item.fixVersion.contents[0])
    
def datetime(dt_str):
    format_str = "%a, %d %b %Y %H:%M:%S +0000"
    time_tup = time.strptime(dt_str, format_str)
    return dt.datetime(*(time_tup[:7]))

#def date(dt_str):
#    return datetime(dt_str).date()
        
def read_created(item):
    return datetime(item.created.contents[0])
    
def read_resolved(item):
    return datetime(item.resolved.contents[0])
