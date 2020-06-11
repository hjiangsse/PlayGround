#!/usr/bin/pythonq

"""
This python script is use to process AM70 Log File
We Cal:
A. pakage lost
B. time spend in sync
C. package process time
Thank you for use! Enjoy your life! Seize the day!
"""

import sys
import re
import getopt

log_path = './logs/log24.txt'
pack_path = './resinv/pack.txt'
inv_pack_path = './res/inv_res.txt'
val_pack_path = './res/val_res.txt'

# Read Am70 Log File Get A Hash Table
# The Key is upDevId + bcastSrcId
# The Value is becastSeqNo List
def ProcFileGetSeqNoHash( file_path ):
    pattern = re.compile(r'(.*)upDevId=(.*) bcastSrcId=(.*) bcastSeqNo=(.*)');
    res_dict = {}
    with open( file_path ) as file:
        for line in file:
            res = pattern.match(line)
            if res:
                key = res.group(2) + '|' + res.group(3)
                if res_dict.has_key(key):
                    res_dict[key].append(res.group(4))
                else:
                    res_dict[key] = [ res.group(4) ]
    return res_dict

# Print out the info inside a dict
def PrintDict( dict ):
    for key in dict.keys():
        print "------------------------------------"
        print key
        print "------------------------------------"

# Give A List
# Cal Package Lost Percentage
# return (lost number, message number)
def CalPackageLostOfList( seqno_list ):
    lost_num = 0
    length = len(seqno_list)
    
    if length == 0:
        return (0, 0, 0)
    elif length == 1:
        return (0, 1, 0)
    else:
        for i in range(1,length):
            diff = int(seqno_list[i]) - int(seqno_list[i - 1]);
            if diff > 1:
                lost_num += diff - 1;

    return (lost_num, length, float(lost_num)/(lost_num + length))

# Give a File
# Cal Package Lost Percentage of a (Device and Process)
# Cal Package Lost Percentage of all
def FinalCalPackLost( res_dict , file):
    lost_sum = 0;
    seq_sum = 0;

    for key in res_dict.keys():
        (lost, seqs, percent) = CalPackageLostOfList( res_dict[key] )
        lost_sum += lost
        seq_sum += seqs
        print >> file, "-------------------------------------"
        print >> file, "key: " + str(key) + " lost number: " + \
            str(lost) + " seq number: " + str(seqs) + \
            " lost percent: " + str(percent)
        print >> file, "-------------------------------------"
    print >> file, "--------------Final Result-------------------"
    print >> file, "lost number: " + str(lost_sum)
    print >> file, "seq number: " + str(seq_sum)
    print >> file, "lost percent: " + \
        str(float(lost_sum) / (lost_sum + seq_sum))
    print >> file, "---------------------------------------------"

# Get Record Time
def GetRecordTime(line):
    if line:
        segs = line.split(' ');
        time_seg = segs[0];
        time_segs = time_seg.split(':')
        return time_segs[1]
    
# Cal Sync Time
def CalSyncTime( file_path ):
    line_list = []
    pattern = re.compile(r'(.*)inst(.*)');
    
    with open( file_path ) as file:
        for line in file:
            res = pattern.match( line )
            if res:
                line_list.append( line )

    last_line = line_list.pop()
    return GetRecordTime( last_line )

# Time String To MilliSecond
def TimeToMilliSec(time):
    if time:
        hour = time[0:2]
        mini = time[2:4]
        secs = time[4:6]
        mili = time[6:8]
        return (hour, mini, secs, mili)

# Get a list which elements are file lines
def GetFileLineList( file_path ):
    line_list = []
    if file_path:
        with open( file_path ) as file:
            for line in file:
                line_list.append( line )
    return line_list

# Proc line list and get INVALID/VALID package
def GetPack( line_list ):
    res_list = []
    inst_pat = re.compile(r'(.*)inst(.*)')
    pack_pat = re.compile(r'(.*)BCAST(.*)')
    for i in range(1, len(line_list)):
        if pack_pat.match(line_list[i - 1]) \
           and pack_pat.match(line_list[i]):
            res = GetRecordTime(line_list[i-1]) + ' ' + 'INV'
            res_list.append(res)
        if pack_pat.match(line_list[i - 1]) \
           and inst_pat.match(line_list[i]):
            res = GetRecordTime(line_list[i-1]) + ' ' + 'VAL'
            res_list.append(res)
    return res_list

# Write a list to file
def WriteListToFile( ulist, file_path):
    if ulist and file_path:
        file_hndl = open(file_path, "w")
        for elem in ulist:
            print >> file_hndl, elem

def GetAllPackFromFile( file_path, res_path ):
    line_list = GetFileLineList(file_path)
    inv_list = GetPack(line_list)
    WriteListToFile( inv_list, res_path )

# Get invalid/valid ratip from inv_log
def GetAllRatio ( inv_path, inv_res_path ):
    inv_num = 0
    val_num = 0
    if inv_path:
        with open(inv_path) as file:
            for line in file:
                segs = line.split(' ')
                flag = segs[1].strip()
                
                if flag == 'INV':
                    inv_num += 1
                if flag == 'VAL':
                    val_num += 1
                    
    res_hndl = open(inv_res_path,"w")
    print >> res_hndl, "-----------------------------------"
    print >> res_hndl, "invaid packages in a day: ",inv_num
    print >> res_hndl, "valid packages in a day: ",val_num
    print >> res_hndl, "invalid ratio: ",str(float(inv_num) / (inv_num + val_num))
    print >> res_hndl, "valid ratio: ",str(float(val_num) / (inv_num + val_num))
    print >> res_hndl, "valid/invalid: ", str(float(val_num) / (inv_num))
    print >> res_hndl, "-----------------------------------"

# Get Valid Package Record From Inv Result
def GetValidRecord(inv_res_path, val_res_path):
    val_file = open(val_res_path,"w")
    val_pat = re.compile(r'(.*)VAL(.*)')
    if inv_res_path and val_res_path:
        with open(inv_res_path) as file:
            for line in file:
                if val_pat.match(line):
                    print >> val_file, line.strip()

# Get InValid Package Record From Inv Result
def GetInValidRecord(inv_res_path, res_path):
    inv_file = open(res_path,"w")
    inv_pat = re.compile(r'(.*)INV(.*)')
    if inv_res_path:
        with open(inv_res_path) as file:
            for line in file:
                if inv_pat.match(line):
                    print >> inv_file, line.strip()
                
def main():
    # parse command line options
    try:
        opts, args = getopt.getopt(sys.argv[1:], "h", ["help"])
    except getopt.error, msg:
        print msg
        print "for help use --help"
        sys.exit(2)

    #process options
    for o, a in opts:
        if o in ("-h", "--help"):
            print __doc__
            sys.exit(0)

    #process arg in args:
    #file_handle = open(res_path, "w");
    #dict = ProcFileGetSeqNoHash( file_path )
    #FinalCalPackLost(dict, file_handle)
    #last_time = CalSyncTime(file_path)
    #TimeToMilliSec(last_time)
    GetAllPackFromFile(log_path, pack_path)
    GetInValidRecord(pack_path, inv_pack_path)
    GetValidRecord(pack_path, val_pack_path)
    
if __name__ == "__main__":
    main()

            

            
