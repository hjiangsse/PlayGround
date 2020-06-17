package main

import (
	"bytes"
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strconv"
)

type FieldInfo struct {
	FieldLen  int
	FieldName string
	FieldType string
	InitVal   string
}

type CfgInfos struct {
	FieldInfos []FieldInfo
}

func main() {
	err := decodeBinaryFile("./genbinary.log", "./config.json", "./res.txt")
	if err != nil {
		panic(err)
	}

	/*
		err := generateBinaryFile("./config.json", "./genbinary.log", 10)
		if err != nil {
			panic(err)
		}
	*/
}

//generateBinaryFile: generate binary file according to the mata file
func generateBinaryFile(metadatapath, outbinarypath string, recordnum int) error {
	//decode the meta data
	var fieldinfos CfgInfos
	cnfJsonFile, err := os.Open(metadatapath)
	if err != nil {
		return err
	}
	cfgBytes, _ := ioutil.ReadAll(cnfJsonFile)

	err = json.Unmarshal(cfgBytes, &fieldinfos)
	if err != nil {
		return err
	}

	//open or create the text file for output
	outfile, err := os.Create(outbinarypath)
	if err != nil {
		return err
	}

	//generate records and write to bianry file
	var tempBuf bytes.Buffer
	var fmtStr string
	var fieldStr string

	for i := 0; i < recordnum; i++ {
		for _, e := range fieldinfos.FieldInfos {
			switch e.FieldType {
			case "int64":
				val, err := strconv.ParseInt(e.InitVal, 10, 64)
				if err != nil {
					return err
				}

				binary.Write(&tempBuf, binary.LittleEndian, &val)
			case "int32":
				val, err := strconv.ParseInt(e.InitVal, 10, 32)
				if err != nil {
					return err
				}

				trval := int32(val)
				binary.Write(&tempBuf, binary.LittleEndian, &trval)
			case "int16":
				val, err := strconv.ParseInt(e.InitVal, 10, 16)
				if err != nil {
					return err
				}

				trval := int16(val)
				binary.Write(&tempBuf, binary.LittleEndian, &trval)
			case "uint64":
				val, err := strconv.ParseUint(e.InitVal, 10, 64)
				if err != nil {
					return err
				}

				binary.Write(&tempBuf, binary.LittleEndian, &val)
			case "uint32":
				val, err := strconv.ParseUint(e.InitVal, 10, 32)
				if err != nil {
					return err
				}

				trval := uint32(val)
				binary.Write(&tempBuf, binary.LittleEndian, &trval)
			case "uint16":
				val, err := strconv.ParseUint(e.InitVal, 10, 16)
				if err != nil {
					return err
				}

				trval := uint16(val)
				binary.Write(&tempBuf, binary.LittleEndian, &trval)
			case "string":
				fmtStr = fmt.Sprintf("%%%ds", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, e.InitVal)
				binary.Write(&tempBuf, binary.LittleEndian, []byte(fieldStr))
			}
		}
		outfile.Write(tempBuf.Bytes())
		tempBuf.Reset()
	}
	return nil
}

//decodeBinaryFile: decode binary file to text file according to the field info
//in the json meta file
func decodeBinaryFile(binaryfilepath, metadatapath, outtextpath string) error {
	//open the binary file
	file, err := os.Open(binaryfilepath)
	if err != nil {
		return err
	}
	defer file.Close()

	//decode the meta data
	var fieldinfos CfgInfos
	cnfJsonFile, err := os.Open(metadatapath)
	if err != nil {
		return err
	}
	cfgBytes, _ := ioutil.ReadAll(cnfJsonFile)

	err = json.Unmarshal(cfgBytes, &fieldinfos)
	if err != nil {
		return err
	}

	//open or create the text file for output
	outfile, err := os.Create(outtextpath)
	if err != nil {
		return err
	}

	//use a buffer to decode every line
	tempBuf := make([]byte, 1024)
	var decodeBuf bytes.Buffer //strore the decoded result
	var fmtStr string
	var fieldStr string
	for {
		for _, e := range fieldinfos.FieldInfos {
			n, err := file.Read(tempBuf[0:e.FieldLen])
			if err != nil {
				if err == io.EOF {
					return nil
				}
				return err
			}

			switch e.FieldType {
			case "int64":
				var val int64
				binary.Read(bytes.NewBuffer(tempBuf[:n]), binary.LittleEndian, &val)
				fmtStr = fmt.Sprintf("%%%dd", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, val)
			case "int32":
				var val int32
				binary.Read(bytes.NewBuffer(tempBuf[:n]), binary.LittleEndian, &val)
				fmtStr = fmt.Sprintf("%%%dd", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, val)
			case "int16":
				var val int16
				binary.Read(bytes.NewBuffer(tempBuf[:n]), binary.LittleEndian, &val)
				fmtStr = fmt.Sprintf("%%%dd", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, val)
			case "uint64":
				var val uint64
				binary.Read(bytes.NewBuffer(tempBuf[:n]), binary.LittleEndian, &val)
				fmtStr = fmt.Sprintf("%%%dd", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, val)
			case "uint32":
				var val uint64
				binary.Read(bytes.NewBuffer(tempBuf[:n]), binary.LittleEndian, &val)
				fmtStr = fmt.Sprintf("%%%dd", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, val)
			case "uint16":
				var val uint16
				binary.Read(bytes.NewBuffer(tempBuf[:n]), binary.LittleEndian, &val)
				fmtStr = fmt.Sprintf("%%%dd", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, val)
			case "string":
				var val = make([]byte, e.FieldLen)
				binary.Read(bytes.NewBuffer(tempBuf[:n]), binary.LittleEndian, val)
				fmtStr = fmt.Sprintf("%%%ds", e.FieldLen)
				fieldStr = fmt.Sprintf(fmtStr, val)
			}

			decodeBuf.WriteString("|" + fieldStr)
		}
		decodeBuf.WriteTo(outfile)
		outfile.WriteString("\n")
	}
	return nil
}
