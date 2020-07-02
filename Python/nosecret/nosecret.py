import struct
import configparser
import sys
import getopt
import codecs
import re
import struct
import random
import os
import shutil
from datetime import datetime
from os import path


random.seed(datetime.now())

snFieldsDict = {
                'T': [(1,6,'pbu'), (6,16,'acct'),(41,46,'branchid'),(46,51,'clrid')],
                'N': [(1,6,'pbu'), (6,16,'acct'),(40,45,'branchid')],
                'C': [(1,6,'pbu')],
                'S': [(1,6,'pbu')],
                'D': [(1,6,'pbu'), (14,24,'acct'), (24,29,'pbu')],
                'A': [(1,11,'acct')],
                'a': [(1,11,'acct')],
                'B': [(1,11,'acct')],
                'b': [(1,11,'acct')],
                'R': [],
                'H': [(6,11,'pbu'), (33,38,'pbu'), (38,48,'acct'), (48,53,'pbu'), (53,63,'acct')],
                'Z': [],
                }
snTypeOffset = 75

ACCTLEN = 9
PBULEN = 5
MEMBERLEN = 5

ACCORIG = list(range(0,ACCTLEN))
ACCORIG_BAK = ACCORIG.copy()
random.shuffle(ACCORIG)

PBUORIG = list(range(0,PBULEN))
PBUORIG_BAK = PBUORIG.copy()
random.shuffle(PBUORIG)

MEMBERORIG = list(range(0,MEMBERLEN))
MEMBERORIG_BAK = PBUORIG.copy()
random.shuffle(MEMBERORIG)

EMPTY = ' '
STAR = '*'
NUMBER = '#'

# -------------------------------------encrypt functions------------------------------
def acctEncrypt(old, charset):    
    accType = chr(old[0])
    accNum  = old[1:]
    newAccNum = ''
    for i in ACCORIG_BAK:
        if isinstance(accNum[ACCORIG[i]], int):
            newAccNum += chr(accNum[ACCORIG[i]])
        else:
            newAccNum += accNum[ACCORIG[i]]
    return bytes(accType + newAccNum, charset)

def numAcctEncrypt(old, charset):
    numAcct, = struct.unpack('<I', old)
    strNumAcct = "%09d" % numAcct
    bytesNumAcct = bytearray(strNumAcct, charset)
    newNumAcctStr = ''
    for i in ACCORIG_BAK:
        if isinstance(bytesNumAcct[ACCORIG[i]], int):
            newNumAcctStr += chr(bytesNumAcct[ACCORIG[i]])
        else:
            newNumAcctStr += bytesNumAcct[ACCORIG[i]]
    newNumAcct = int(newNumAcctStr)
    new = struct.pack('<I', newNumAcct)
    return new

def pbuEncrypt(origPbu, charset):
    newPbu = '';
    for i in PBUORIG_BAK:
        if isinstance(origPbu[PBUORIG[i]], int):
            newPbu += chr(origPbu[PBUORIG[i]])
        else:
            newPbu += origPbu[PBUORIG[i]]
    return bytes(newPbu, charset)

def memberEncrypt(old, charset):
    validMember = old[7:]
    newValidMember = ''
    for i in MEMBERORIG_BAK:
        if isinstance(validMember[MEMBERORIG[i]], int):
            newValidMember += chr(validMember[MEMBERORIG[i]])
        else:
            newValidMember += validMember[MEMBERORIG[i]]
    return old[0:7] + bytes(newValidMember, charset)

def hyjcEncrypt(old, charset):
    newHyjc = EMPTY * len(old)
    return bytes(newHyjc, charset)

def jgdmEncrypt(old, charset):
    newJgdm = EMPTY * len(old)
    return bytes(newJgdm, charset)

def jgmcEncrypt(old, charset):
    newJgmc = EMPTY * len(old)
    return bytes(newJgmc, charset)

def gsmcEncrypt(old, charset):
    newGsmc = EMPTY * len(old)
    return bytes(newGsmc, charset)

def branchidEncrypt(old, charset):
    newBranchId = EMPTY * len(old)
    return bytes(newBranchId, charset)

def interOrdNumEncrypt(old, charset):
    newOrdNum = EMPTY * len(old)
    return bytes(newOrdNum, charset)

def interTextEncrypt(old, charset):
    newInterText = EMPTY * len(old)
    return bytes(newInterText, charset)

def clridEncrypt(old, charset):
    newClrIdText = EMPTY * len(old)
    return bytes(newClrIdText, charset)

def szbcanEncrypt(old, charset):
    newSzBcanText = EMPTY * len(old)
    return bytes(newSzBcanText, charset)

def szdactEncrypt(old, charset):
    newSzDactText = EMPTY * len(old)
    return bytes(newSzDactText, charset)

def szymtEncrypt(old, charset):
    newSzYmtText = EMPTY * len(old)
    return bytes(newSzYmtText, charset)

fieldEncryMatrix = {
    'acct': acctEncrypt,
    'numacct': numAcctEncrypt,
    'pbu': pbuEncrypt,
    'member':memberEncrypt,
    'jgdm':jgdmEncrypt,
    'jdmc':jgmcEncrypt,
    'gsmc':gsmcEncrypt,
    'branchid':branchidEncrypt,
    'interordnum':interOrdNumEncrypt,
    'intertext': interTextEncrypt,
    'clrid': clridEncrypt,
    'szbcan': szbcanEncrypt,
    'szdact': szdactEncrypt,
    'szymt': szymtEncrypt,
    'hyjc': hyjcEncrypt,
}
# -------------------------------------encrypt functions------------------------------

# process SN File
def processSnFile(inpath, outpath, recsize):
    print('-' * 80)
    print("process " + inpath + " start...")

    if not path.exists(inpath):
        print("file " + inpath + "do not exists, check it! Bye!")
        exit(1)
        
    with open(inpath, "rb") as file, open(outpath, "wb+") as output:
        bytes = file.read(int(recsize))
        while bytes:
            #get message body type
            msgType = bytes[snTypeOffset - 1]
            chrMsgType = chr(msgType)

            if chrMsgType in snFieldsDict:
                for fieldInfo in snFieldsDict[chrMsgType]:
                    curfield = bytes[snTypeOffset + fieldInfo[0] -1:snTypeOffset + fieldInfo[1] -1]
                    fieldType = fieldInfo[2]
                    
                    encryptedfield = b''
                    procfunc = fieldEncryMatrix.get(fieldType, 'unknow field')
                    if procfunc != 'unknow field':
                        encryptedfield = procfunc(curfield, 'utf-8')
                        bytes = bytes[:fieldInfo[0] -1] + encryptedfield + bytes[fieldInfo[1]-1:]
                
                output.write(bytes)
            bytes = file.read(int(recsize))
    print("generate " + outpath + " finish...")
    print('-' * 80)

# process bianry file, mess the accout field
def processBinaryFile(inpath, outpath, recsize ,fieldnum, cfginfo):
    print('-' * 80)
    print("process " + inpath + " start...")

    if not path.exists(inpath):
        print("file " + inpath + "do not exists, check it! Bye!")
        exit(1)
    
    with open(inpath, "rb") as file, open(outpath, "wb+") as output:
        bytes = file.read(int(recsize))
        while bytes:
            for i in range (1, int(fieldnum) + 1):
                startoffset = int(cfginfo["[Field" + str(i) + "]"][0])
                endoffset = int(cfginfo["[Field" + str(i) + "]"][1])
                fieldType = cfginfo["[Field" + str(i) + "]"][2].strip()

                curfield = bytes[startoffset - 1:endoffset -1]

                encryptedfield = b''
                procfunc = fieldEncryMatrix.get(fieldType, 'unknow field')
                if procfunc != 'unknow field':
                    encryptedfield = procfunc(curfield, 'utf-8')
                    bytes = bytes[:startoffset-1] + encryptedfield + bytes[endoffset-1:]
                    
            output.write(bytes)
            bytes = file.read(int(recsize))
    print("generate " + outpath + " finish...")
    print('-' * 80)

# process bianry file, mess the accout field
def processTextFile(inpath, outpath, recsize ,fieldnum, cfginfo, charset):
    print('-' * 80)
    print("process " + inpath + " start...")

    if not path.exists(inpath):
        print("file " + inpath + "do not exists, check it! Bye!")
        exit(1)

    file = codecs.open(inpath, mode='r', encoding=charset, errors='strict', buffering=-1)
    output = codecs.open(outpath, mode='w+', encoding=charset, errors='strict', buffering=-1)

    fieldsInfos = []
    for i in range (1, int(fieldnum) + 1):
            startoffset = int(cfginfo["[Field" + str(i) + "]"][0])
            endoffset = int(cfginfo["[Field" + str(i) + "]"][1])
            fieldType = cfginfo["[Field" + str(i) + "]"][2].strip()
            fieldsInfos.append((startoffset, endoffset, fieldType))
    
    bytes = file.read(int(recsize))
    while bytes:
        for i in range (0, int(fieldnum)):
            startoffset = fieldsInfos[i][0]
            endoffset = fieldsInfos[i][1]
            fieldType = fieldsInfos[i][2].strip()
            
            curfield = bytes[startoffset - 1:endoffset -1]
            print(curfield, fieldType)
            bytesCurfield = bytearray(curfield, charset)
                
            encryptedfield = b''
            procfunc = fieldEncryMatrix.get(fieldType, 'unknow field')
            if procfunc != 'unknow field':
                encryptedfield = procfunc(bytesCurfield, charset)
                bytes = bytes[:startoffset - 1] + encryptedfield.decode(charset) + bytes[endoffset - 1:]
                
        output.write(bytes)
        bytes = file.read(int(recsize))

    file.close()
    output.close()
    print("generate " + outpath + " finish...")
    print('-' * 80)
            
# get fields information(for bianry and text file)
def getFieldsInfo(infostr):
    infolines = infostr.splitlines()
    fieldnum = infolines[0].strip()
    resDict = {}
    
    tempKey = ''
    for line in infolines[1:]:
        pureLine = line.strip()
        if pureLine == "":
            continue
        if pureLine.find("Offset") != -1 or pureLine.find("Type") != -1:
            segs = pureLine.split('=')
            resDict[tempKey] += [segs[1]]
        else:
            resDict[pureLine] = []
            tempKey = pureLine
    return fieldnum, resDict

# get read user input file based on configured path
def getUserRealFilePath(confpath):
    basename = path.basename(confpath)
    dirname  = path.dirname(confpath)

    outflag = 0
    if "output" in dirname:
        dirname = dirname.replace("output", "input")
        outflag = 1

    namestart = basename
    
    if "yyyymmdd" in basename:
        namestart = basename.split("yyyymmdd")[0]
    if "YYYYMMDD" in basename:
        namestart = basename.split("YYYYMMDD")[0]
    if "mmdd" in basename and "yyyymmdd" not in basename:
        namestart = basename.split("mmdd")[0]
    if "MMDD" in basename and "YYYYMMDD" not in basename:
        namestart = basename.split("MMDD")[0]
        

    for root, dirs, files in os.walk(dirname):
        for file in files:
            if namestart in file:
                if outflag == 1:
                    return path.join(dirname.replace("input", "output"), file)
                else:
                    return path.join(dirname, file)
    return ""
            
            
# the main function of this program
def main(argv):
    inputfile = ''

    try:
        opts, args = getopt.getopt(argv, "hc:", ["cfile=",])
    except getopt.GetoptError:
        print("Usage: nosecret.py -c <configfile>")
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print("Usage: nosecret.py -c <configfile>")
            print("Caution: FieldStartOffset and FieldEndOffset start from 1, ")
            sys.exit()
        elif opt in ("-c", "--cfile"):
            configfile = arg
    
    config = configparser.ConfigParser()
    config.read(configfile)

    # get total file number in config file
    totalFileNum = int(config['FileNum']['totalfilenum'])

    # process each file config
    for i in range(1,totalFileNum+1):
        curFileIndex = "File" + str(i)
        curFileCfgInfo = config[curFileIndex]
    
        curFileType = curFileCfgInfo['FileType']
        
        curFileInPath = curFileCfgInfo['FileInPath']
        realInputPath = getUserRealFilePath(curFileInPath)
        if realInputPath == "":
            print("Can Not Find A File [" + curFileInPath + "], But It Is In Config File!");
            continue
            
        curFileOutPath = curFileCfgInfo['FileOutPath']
        realOutputPath = getUserRealFilePath(curFileOutPath)
        if path.exists(realOutputPath):
            os.remove(realOutputPath)

        curFileRecSize = curFileCfgInfo['FileRecSize']
        curFileInfos = curFileCfgInfo['FieldNum']
        curEncoding  = curFileCfgInfo['FileCharSet']
        fieldsNum, fieldsInfo = getFieldsInfo(curFileInfos)

        #if no field need to be encrypted, just copy file and continue
        if (curFileType == "BIN" or curFileType == "TXT") and fieldsNum == "0":
            shutil.copyfile(realInputPath, realOutputPath)
            continue
                
        
        if curFileType == "BIN" or curFileType == "TC":
            processBinaryFile(realInputPath, realOutputPath, curFileRecSize, fieldsNum, fieldsInfo)
        elif curFileType == "TXT":
            processTextFile(realInputPath, realOutputPath, curFileRecSize, fieldsNum, fieldsInfo, curEncoding)
        elif curFileType == "SN":
            processSnFile(realInputPath, realOutputPath, curFileRecSize)

if __name__ == "__main__":
    # execute only if run as a script
    main(sys.argv[1:])
