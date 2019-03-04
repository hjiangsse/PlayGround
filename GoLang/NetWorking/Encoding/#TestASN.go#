package main

import (
	"fmt"
	"encoding/asn1"
	"os"
)

type Student struct {
	Name string
	Age  int
}

func main() {
	srcStruct := Student{"hjiang", 29}

	mdata, err := asn1.Marshal(srcStruct)
	checkError(err)

	fmt.Printf("mdata %v type is %T\n", mdata, mdata)

	var dstStruct Student
	_, err1 := asn1.Unmarshal(mdata, &dstStruct)
	checkError(err1)

	fmt.Println("After marshal/unmarshal: ", dstStruct)
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
