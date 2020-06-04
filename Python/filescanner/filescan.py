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
    return r"%s%s%s" % ("^(.*?(\\b", key, "\\b)[^$]*)$")

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
    if os.path.exists(resfilename):
        os.remove(resfilename)
    
    resfilefd = open(resfilename, "w")
    filemap[key] = resfilefd

# match a line
def match_line(fname, cnt, line):
    ismatch = False
    words = line.split()
    for word in words:
        for key in configs["keys"]:
            if "word" in configs["scanmode"]:
                matchobj = re.search(key, word)
                if matchobj and key in filemap:
                    filemap[key].write("%s,%d,%s" % (fname, cnt, line))
                    ismatch = True
            if "contain" in configs["scanmode"]:
                if word.find(key.encode('utf-8').strip()) != -1:
                    filemap[key].write("%s,%d,%s" % (fname, cnt, line))
                    ismatch = True
                
        if ismatch:
            # already find a word match in current line, do not
            # match following words
            break
    
    
# walk recursively 
for root, subdirs, files in os.walk(configs["srcpath"]):
    # travese all files
    for filename in files:
        if filename == "configs.json" or filename in configs["filtername"]:
            continue
        fname = os.path.join(root, filename)
        # match all keywords for current file lines
        with open(fname) as fp:
            for cnt, line in enumerate(fp):
                match_line(fname, cnt, line)
                            
# close all the match result file
for key in filemap:
    filemap[key].close()
