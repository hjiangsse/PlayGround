#!/usr/bin/python
import sys
import os
import re
import json

# load config file set by user
def load_configs(cfgpath) :
    with open(cfgpath) as cf:
        return json.load(cf)

# only search key as a word
def trans_regex(key):
    return "%s%s%s" % ("^(.*?(\\b", key, "\\b)[^$]*)$")


configs = load_configs("./configs.json")
    
# dict to storage file discriptor
filemap = {}

# create files strore the match results, store fds in a dict
for key in configs["keys"] :
    if not os.path.isdir(configs["respath"]):
        try:
            os.mkdir(configs["respath"])
        except OSError:
            sys.exit("Creation of the directory %s failed" % configs["respath"])
        
    resfilename = os.path.join(configs["respath"], key + ".txt")
    resfilefd = open(resfilename, "w")
    filemap[key] = resfilefd

# walk recursively 
for root, subdirs, files in os.walk(configs["srcpath"]):
    # travese all files
    for filename in files:
        if filename == "configs.json" or filename in configs["filtername"]:
            continue
        fname = os.path.join(root, filename).encode('utf-8').strip()
        # match all keywords for current file lines
        with open(fname) as fp:
            for cnt, line in enumerate(fp):
                for key in configs["keys"]:
                    if "word" in configs["scanmode"]:
                        matchobj = re.search(trans_regex(key), line)
                        if matchobj and key in filemap:
                            filemap[key].write("%s,%d,%s" % (fname, cnt, line))
                    if "contain" in configs["scanmode"]:
                        if key in line:
                            filemap[key].write("%s,%d,%s" % (fname, cnt, line))
                            
# close all the match result file
for key in filemap:
    filemap[key].close()
