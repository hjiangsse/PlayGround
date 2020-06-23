'''
from struct import *

snFieldsDict = {
                'T': [(49,59,'acct'), (44,49,'pbu')],
                'N': [(49,59,'acct'), (44,49,'pbu')],
                'C': [(44,49,'pbu')],
                'S': [],
                'D': [(44,49,'pbu'), (57,67,'acct')],
                'A': [(44,54,'acct')],
                'a': [(44,54,'acct')],
                'B': [(44,49,'pbu')],
                'b': [(44,49,'pbu')],
                'R': [],
                'H': [(49,54,'pbu'), (76,81,'pbu'), (81,91,'acct'), (91,96,'pbu'), (96, 106, 'acct')],
                'Z': [],
                }
snTypeOffset = 43

def acctprocess():
    print("This is acct processing function")

def pbuprocess():
    print("This is pbu processing function")

def flagprocess():
    print("This is flag processing function")

switcher = {
    'acct': acctprocess,
    'pbu': pbuprocess,
    'flag': flagprocess,
}


head = pack('c15sQ8sQHc', b'N', b'',12345, b'20201023',12345,5,b'T')
tradeBody1 = pack('5s10s16s', b'12435', b'A123456789', b'12345')
tradeBody2 = pack('Qc', 1,b'C')
tradeBody3 = pack('5s5s', b'21111', b'43215')
tradeBody4 = pack('H', 1)
tradeBody5 = pack('Q', 2134)
tradeBody6 = pack('6sH', b'600000',13)
tradeBody7 = pack('qqq9s', 50000,100,1000,b'')
print("----------------")
print(len(tradeBody1))
print(len(tradeBody2))
print(len(tradeBody3))
print(len(tradeBody4))
print(len(tradeBody5))
print(len(tradeBody6))
print(len(tradeBody7))
print("----------------")

tradeBody = tradeBody1 + tradeBody2 + tradeBody3 + tradeBody4 + tradeBody5 + tradeBody6 + tradeBody7
print(len(tradeBody))

oneMsg = head + tradeBody

msgType = chr(oneMsg[snTypeOffset-1])
if msgType in snFieldsDict:
    fieldsInfo = snFieldsDict[msgType]
    for info in fieldsInfo:
        startoffset = info[0]
        endoffset = info[1]
        print(oneMsg[startoffset-1:endoffset-1])
        fmtstr = "<%ds" % (endoffset - startoffset)
        stracc = unpack(fmtstr, oneMsg[startoffset-1:endoffset-1])[0]
        print(stracc)

func = switcher.get('haha', "Invalid Para")
if func != 'Invalid Para':
    print(type(func))
    func()
        
with open("./sn_input.log", "wb") as output:
    for i in range (4000):
        output.write(oneMsg)
'''

import random
from datetime import datetime
random.seed(datetime.now())

ORIG = list(range(0,9))
ORIG_BAK = ORIG.copy()
print(ORIG)
random.shuffle(ORIG)
print(ORIG)


def accEncrypt(old):
    accType = old[0]
    accNum  = old[1:]
    newAccNum = '';
    for i in ORIG_BAK:
        newAccNum += chr(accNum[ORIG[i]])
    return bytearray(newAccNum, 'utf-8')

print(accEncrypt(b'A123456789'))
print(accEncrypt(b'A123456789'))
print(accEncrypt(b'A123456788'))
