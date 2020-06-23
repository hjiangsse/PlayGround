import configparser
import sys
import getopt
import codecs
import re
import struct
import random
from datetime import datetime

random.seed(datetime.now())

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
                'H': [(49,54,'pbu'), (76,81,'pbu'), (81,91,'acct'), (91,96,'pbu'), (96,106, 'acct')],
                'Z': [],
                }
snTypeOffset = 43

ACCTLEN = 9
PBULEN = 5

ACCORIG = list(range(0,ACCTLEN))
ACCORIG_BAK = ACCORIG.copy()
random.shuffle(ACCORIG)

PBUORIG = list(range(0,PBULEN))
PBUORIG_BAK = PBUORIG.copy()
random.shuffle(PBUORIG)

# encrypt account
def acctEncrypt(old):
    accType = str(old[0])
    accNum  = old[1:]
    newAccNum = '';
    for i in ACCORIG_BAK:
        if isinstance(accNum[ACCORIG[i]], int):
            newAccNum += chr(accNum[ACCORIG[i]])
        else:
            newAccNum += accNum[ACCORIG[i]]
    return bytes(accType + newAccNum, 'utf-8')

def pbuEncrypt(origPbu):
    newPbu = '';
    for i in PBUORIG_BAK:
        if isinstance(origPbu[PBUORIG[i]], int):
            newPbu += chr(origPbu[PBUORIG[i]])
        else:
            newPbu += origPbu[PBUORIG[i]]
    return bytes(newPbu, 'utf-8')

fieldEncryMatrix = {
    'acct': acctEncrypt,
    'pbu': pbuEncrypt,
}

# process SN File
def processSnFile(inpath, outpath, recsize):
    print("process " + inpath + " start...")
    with open(inpath, "rb") as file, open(outpath, "wb") as output:
        bytes = file.read(int(recsize))
        while bytes:
            #get message body type
            msgType = bytes[snTypeOffset - 1]
            chrMsgType = chr(msgType)

            if chrMsgType in snFieldsDict:
                for fieldInfo in snFieldsDict[chrMsgType]:
                    curfield = bytes[fieldInfo[0]-1:fieldInfo[1]-1]
                    fieldType = fieldInfo[2]
                    
                    encryptedfield = b''
                    procfunc = fieldEncryMatrix.get(fieldType, 'unknow field')
                    if procfunc != 'unknow field':
                        encryptedfield = procfunc(curfield)
                        bytes = bytes[:fieldInfo[0] -1] + encryptedfield + bytes[fieldInfo[1]-1:]
                
                output.write(bytes)
            bytes = file.read(int(recsize))
    print("generate " + outpath + " finish...")
            

# process bianry file, mess the accout field
def processBinaryFile(inpath, outpath, recsize ,fieldnum, cfginfo):
    print("process " + inpath + " start...")
    with open(inpath, "rb") as file, open(outpath, "wb") as output:
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
                    encryptedfield = procfunc(curfield)
                    bytes = bytes[:startoffset-1] + encryptedfield + bytes[endoffset-1:]
                    
            output.write(bytes)
            bytes = file.read(int(recsize))
    print("generate " + outpath + " finish...")

# process bianry file, mess the accout field
def processTextFile(inpath, outpath, recsize ,fieldnum, cfginfo):
    print("process " + inpath + " start...")
    with open(inpath, "r") as file, open(outpath, "w") as output:
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
                    encryptedfield = procfunc(curfield)
                    bytes = bytes[:startoffset - 1] + encryptedfield.decode('utf-8') + bytes[endoffset - 1:]
                
            output.write(bytes)
            bytes = file.read(int(recsize))
    print("generate " + outpath + " finish...")
            
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

# the main function of this program
def main(argv):
    inputfile = ''
    outputfile = ''
    configfile = ''

    try:
        opts, args = getopt.getopt(argv, "hc:i:o", ["cfile=", "ifile=", "ofile="])
    except getopt.GetoptError:
        print("Usage: nosecret.py -c <configfile> -i <inputfile> -o <outputfile>")
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print("Usage: nosecret.py -c <configfile> -i <inputfile> -o <outputfile>")
            print("Caution: FieldStartOffset and FieldEndOffset start from 1, ")
            sys.exit()
        elif opt in ("-c", "--cfile"):
            configfile = arg
        elif opt in ("-i", "--ifile"):
            inputfile = arg
        elif opt in ("-o", "--ofile"):
            outputfile = arg
    
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
        curFileOutPath = curFileCfgInfo['FileOutPath']
        curFileRecSize = curFileCfgInfo['FileRecSize']
        curFileInfos = curFileCfgInfo['FieldNum']
        fieldsNum, fieldsInfo = getFieldsInfo(curFileInfos)
        
        if curFileType == "BIN":
            processBinaryFile(curFileInPath, curFileOutPath, curFileRecSize, fieldsNum, fieldsInfo)
        elif curFileType == "TXT":
            processTextFile(curFileInPath, curFileOutPath, curFileRecSize, fieldsNum, fieldsInfo)
        else:
            processSnFile(curFileInPath, curFileOutPath, curFileRecSize)

if __name__ == "__main__":
    # execute only if run as a script
    main(sys.argv[1:])
