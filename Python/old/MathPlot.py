import matplotlib.pyplot as plt
import numpy as np

val_file_path = './res/val_res.txt'
inv_file_path = './res/inv_res.txt'

def ProcFileGetTime(file_path):
    time_list = []
    if file_path:
        with open(file_path) as file:
            for line in file:
                segs = line.split(' ')
                time_list.append(segs[0])
    return time_list

def ProcTimeListGetMinList(time_list):
    mini_list = []
    if time_list:
       for time in time_list:
           str_min = '0'
           str_hour = '0'
           if len(time) == 6:
               str_min = time[0:2]
           if len(time) == 7:
               str_hour = time[0]
               str_min = time[1:3]
           if len(time) == 8:
               str_hour = time[0:2]
               str_min = time[2:4]
           mini_list.append(int(str_hour) * 60 + int(str_min))
    return mini_list

def ReadValGetMin(val_path, min_path):
    if val_path and min_path:
        time_list = ProcFileGetTime(val_path)
        min_list = ProcTimeListGetMiniList(time_list)

        out = open(min_path,"w")
        for min in min_list:
            print >> out, min

#ReadValGetMin(val_file_path, min_file_path)
    
time_val_list = ProcFileGetTime(val_file_path)
min_val_list = ProcTimeListGetMinList(time_val_list)

time_inv_list = ProcFileGetTime(inv_file_path)
min_inv_list = ProcTimeListGetMinList(time_inv_list)
                   
plt.hist( [min_inv_list, min_val_list] , bins=30, normed=True)
plt.title("Valid Package Distribute")
plt.xlabel("time")
plt.ylabel("Frequency")
plt.show()
