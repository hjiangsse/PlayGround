#!/usr/bin/python
import sys
import os
import re

if len(sys.argv) < 3 :
    print("Usage: ./filescan.py path keyword...")

# dict to storage file discriper
filemap = {}

# create files strore the match results, store fds in a dict
for key in sys.argv[2:] :
    resfilename = key + ".txt"
    resfilefd = open(resfilename, "w")
    filemap[key] = resfilefd

# walk recursively 
for root, subdirs, files in os.walk(sys.argv[1]):
    # travese all files
    for filename in files:
        fname = os.path.join(root, filename)
        #do match for all keywords in current file lines
        with open(fname) as fp:
            for cnt, line in enumerate(fp):
                for key in sys.argv[2:]:
                    matchobj = re.search(key, line, re.I)
                    if matchobj and key in filemap:
                            filemap[key].write("%s:%d,%s" % (fname, cnt, line))
                            
# close all the match resulf file
for key in filemap:
    filemap[key].close()
