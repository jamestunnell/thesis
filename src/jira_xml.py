# -*- coding: utf-8 -*-
from BeautifulSoup import BeautifulSoup
import datetime as dt
import dateutil.parser as duparser

STATUSES = { 1: "open", 5: "resolved", 6: "closed",
            10018: "inreview", 10019: "userinput"}
RESOLUTION_FIXED = 1
RESOLUTION_COMPLETE = 8
RESOLUTION_DONE = 9
RESOLUTIONS = { -1:"unresolved", 1: "fixed", 2:"wontfix", 3:"duplicate",
               4:"incomplete", 5:"norepro", 6:"asdesigned",
               7:"goneaway", 8:"complete", 9:"done"}

class Issue:
    def __init__(self, item_xml, type_map):
        self.type_map = type_map
        self.key = read_key(item_xml)
        self.type = read_type(item_xml)
        self.priority = read_priority(item_xml)
        self.created  = read_created(item_xml)
        self.resolved  = read_resolved(item_xml)
        self.resolution = read_resolution(item_xml)
        self.status = read_status(item_xml)
        self.subtasks = read_subtasks(item_xml)
        self.fixversion = read_fixversion(item_xml)
        
    def type_str(self):
        if(not self.type in self.type_map):
            print("Type id %d not found" % self.type)
        return self.type_map[self.type]
        
    def status_str(self):
        if(not self.status in STATUSES):
            print("Status id %d not found" % self.status)
        return STATUSES[self.status]
        
    def resolution_str(self):
        if(not self.resolution in RESOLUTIONS):
            print("Resolution id %d not found" % self.resolution)
        return RESOLUTIONS[self.resolution]        
    
    def isfixed(self):
        return self.resolution == RESOLUTION_FIXED or \
            self.resolution == RESOLUTION_COMPLETE or \
            self.resolution == RESOLUTION_DONE

def load_issues(xml_fname, type_map):
    f = file(xml_fname)
    bs = BeautifulSoup(f.read())
    items = bs.channel.findAll("item")
    return [ Issue(i, type_map) for i in items ]

def read_key(item):
    return int(item.key['id'])

def read_type(item):
    return int(item.type['id'])

def read_priority(item):
    pri = item.find('priority')
    if pri:
        return int(pri['id'])
    else:
        return "NA"

def read_subtasks(item):
    return [ int(i['id']) for i in item.subtasks.findAll("subtask") ]

def read_resolution(item):
    return int(item.resolution['id'])
    
def read_status(item):
    return int(item.status['id'])
    
def read_fixversion(item):
    if item.fixversion:
        return str(item.fixversion.contents[0] or "NA")
    else:
        return "NA"

def datetime(jira_dt_str):
    return duparser.parse(jira_dt_str)

def read_created(item):
    return datetime(item.created.contents[0])
    
def read_resolved(item):
    if item.resolved:
        return datetime(item.resolved.contents[0])
    else:
        return "NA"
