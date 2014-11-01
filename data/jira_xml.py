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

def load_items(xml_fname):
    f = file(xml_fname)
    bs = BeautifulSoup(f.read())
    return bs.channel.findAll("item")

def isfixed(item):
    return int(item.resolution["id"]) == RESOLUTION_FIXED

def isclosed(item):
    return int(item.status["id"]) == STATUS_CLOSED
    
def keep_fixed(items):
    return filter(lambda x: isfixed(x), items)

def keep_closed(items):
    return filter(lambda x: isclosed(x), items)

def read_type(item):
    return TYPES[int(item.type['id'])]

def read_priority(item):
    return int(item.priority['id'])
    
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
