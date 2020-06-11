#!usr/bin/python
import matplotlib.pyplot as plt
import numpy as np
import re

log_file_path = './logs/log27.txt'
time_file_path = './time/time27.txt'
avg_file_path = './time/avg27.txt'

#------------------MACRO DEFIN--------------#
TIME_LEN_SHORT  = 6
TIME_LEN_MIDDLE = 7
TIME_LEN_LONG   = 8

HOUR_TO_MILLI = 3600000
MINU_TO_MILLI = 60000
SECO_TO_MILLI = 1000
MILL_EXP = 10
#-------------------------------------------#

#------------------UTIL FUNCTIONS-----------#

# Process AM70 orignal log file
# Get Package Lines
# The Lines will be change to another mode
def ProcSourceLogGetPack(log_file_path):
    pack_list = []
    if log_file_path:
        with open(log_file_path) as file:
            for line in file:
                if IsPackLine(line):
                    pack_list.append(ExtrMsgFromLine(line))
    return pack_list

# Judge if a line is a package line
def IsPackLine(line):
    if line:
        pack_pat = re.compile(r'(.*)BCAST(.*)')
        if pack_pat.match(line):
            return True
        else:
            return False
    return False

# Extract Message in a package line
# The Result is a dict
# time ->
# upDevId ->
# bcastSrcId ->
# bcastSeqNo ->
def ExtrMsgFromLine(line):
    res_dict = {}
    if line:
        pure_line = line.strip()
        pure_line = re.sub(r'\[.*\]',"",pure_line)
        line_segs = pure_line.split(' ')
        for seg in line_segs:
            sub_segs = []
            if re.match(r'(.*):(.*)',seg):
                sub_segs = seg.split(':')
            if re.match(r'(.*)=(.*)',seg):
                sub_segs = seg.split('=')
            res_dict[sub_segs[0]] = sub_segs[1]
    return res_dict

#Conver a string time to millisecond
def ConvTimeStr(time):
    (hour,minute,second,milli) = [0,0,0,0]
    if time:
        if len(time) == TIME_LEN_SHORT:
            minute = int(time[0:2])
            second = int(time[2:4])
            milli  = int(time[4:6])
        if len(time) == TIME_LEN_MIDDLE:
            hour = int(time[0])
            minute = int(time[1:3])
            second = int(time[3:5])
            milli = int(time[5:7])
        if len(time) == TIME_LEN_LONG:
            hour = int(time[0:2])
            minute = int(time[2:4])
            second = int(time[4:6])
            milli = int(time[6:8])
    return hour * HOUR_TO_MILLI + \
           minute * MINU_TO_MILLI + \
           second * SECO_TO_MILLI + \
           milli * MILL_EXP

#Print Time ImterVal into File
def PrTmToFile(time_list, file_path):
    if time_list and file_path:
        file = open(file_path,"w")
        for time in time_list:
            print >> file, time
            
#----------------------------------------------#

#-------------Business Functions---------------#
# prev_pack is a dictionary
# next_pack is a dictionary
# return type is:
# [84|xxxxxxxx|xxxx] --> time_interval
def CalTwoPackTimeDiff(prev_pack, next_pack):
    time_inter = 0
    info_str = ""
    if prev_pack and next_pack:
        info_str = "[" + prev_pack['upDevId'] + \
                   "|" + prev_pack['bcastSrcId'] + \
                   "|" + prev_pack['bcastSeqNo'] + "]"
        time_inter = ConvTimeStr(next_pack['time']) - \
                     ConvTimeStr(prev_pack['time'])
    return (prev_pack['time'], info_str, time_inter)

# get time interval
def GetTimeInter(lines_info):
    inter_res = []
    for index in range(0,len(lines_info) - 1):
        (time, info, inter) = CalTwoPackTimeDiff(lines_info[index], lines_info[index + 1])
        inter_res.append([time, info, inter])
    return inter_res

# filter out interval list
def FilterInterval(all_info_list):
    inter_list = []
    if all_info_list:
        for info in all_info_list:
            inter_list.append( info[2] )
    return inter_list

# Cal Avrage of A number list
def CalAvgNumList(num_list):
    if num_list:
        return reduce(lambda x,y: x + y, num_list) / len(num_list)
#----------------------------------------------#

#-------------Test Functions-------------------#
def TestPrtLineRes(pack_list):
    for line_info in pack_list:
        print line_info

lines_info = ProcSourceLogGetPack(log_file_path)
inter_res = GetTimeInter(lines_info)
time_list = FilterInterval(inter_res)
file = open(avg_file_path,"w")
print >> file, CalAvgNumList(time_list)

#PrTmToFile(inter_res, time_file_path)
#time_str = "11010101"
#print ConvTimeStr(time_str)

#----------------------------------------------#
