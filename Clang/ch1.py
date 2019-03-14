#!/usr/bin/python3
import random as ran
import BitVector

ran.seed();

#generate a unique number set, k is the size,
#every number in this set is less than n
def gen_num_set (k,n):
    tmp_set = set();
    while len(tmp_set) < k :
        tmp_set.add(int(ran.random() * n));
    return tmp_set

#write a set to a file, s is the set,
#fname is the file name
def save_num_set (s, fname):
    #open the fname file
    fo = open(fname, "w+");
    if fo :
        for i in s :
            fo.write(str(i) + "\n");

my_set = gen_num_set(10000000,10000000);
#print(ran.shuffle(sorted(my_set)));
save_num_set(my_set, "test.txt");

#python bit array
