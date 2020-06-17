package main

import (
	"fmt"
)

func main() {
	var num uint32 = 0x12345678
	fmt.Println(&num)
	num = num << 8
	fmt.Printf("%x\n", num)
}
