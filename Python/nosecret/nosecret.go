package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("input.log")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	fldBuff := make([]byte, 8)
	_, err = file.Read(fldBuff)
	if err != nil {
		panic(err)
	}

	var res uint64
	buf := bytes.NewReader(fldBuff)
	err = binary.Read(buf, binary.LittleEndian, &res)
	if err != nil {
		panic(err)
	}

	fmt.Println(res)
}
