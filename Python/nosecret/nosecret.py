import configparser
import sys
import getopt
import codecs
import re
import struct

# encrypt account
def accountEncrypt(origAcct, addNum):
    acctType = origAcct[0]
    acctNum = origAcct[1:]
    reversedNum = acctNum[::-1]
    newNum = (int(reversedNum) + addNum) % pow(10, 9)
    newStrNum = str(newNum)
    return chr(acctType) + newStrNum

# process bianry file, mess the accout field
def processBinaryFile(inpath, outpath, recsize ,fieldnum, cfginfo, addnum):
    with open(inpath, "rb") as file, open(outpath, "wb") as output:
        bytes = file.read(int(recsize))
        while bytes:
            for i in range (1, int(fieldnum) + 1):
                startoffset = int(cfginfo["[Field" + str(i) + "]"][0])
                endoffset = int(cfginfo["[Field" + str(i) + "]"][1])
                curacc = bytes[startoffset - 1:endoffset -1]
                
                fmtstr = "<%ds" % (endoffset - startoffset)
                stracc = struct.unpack(fmtstr, curacc)[0]
                encryptedAcct = accountEncrypt(stracc, addnum)

                #write the encrypted account back to the stream
                packfmtstr = "<%ds" % (endoffset - startoffset)
                packedEncryAcct = struct.pack(packfmtstr, bytearray(encryptedAcct, 'utf-8'))
                bytes = bytes[:startoffset-1] + packedEncryAcct + bytes[endoffset-1:]
                
            #write the new byte to the output
            output.write(bytes)

            #read the next block of record
            bytes = file.read(int(recsize))

# get accouts fields information
def getFieldsInfo(infostr):
    infolines = infostr.splitlines()
    
    fieldnum = infolines[0].strip()
    resDict = {}
    
    tempKey = ''
    for line in infolines[1:]:
        pureLine = line.strip()
        if pureLine == "":
            continue
        
        if pureLine.find("Offset") != -1:
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
    addNum = int(config['Common']['AddNum'])
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
        accFieldsNum, FieldsInfo = getFieldsInfo(curFileInfos)

        if curFileType == "BIN":
            processBinaryFile(curFileInPath, curFileOutPath, curFileRecSize, accFieldsNum, FieldsInfo, addNum)

if __name__ == "__main__":
    # execute only if run as a script
    main(sys.argv[1:])
