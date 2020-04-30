package main

import (
	"fmt"
	"net"
	"os"
	"bufio"
	"strings"
	"bytes"
)

//strings used by the user interface
const (
	uiDir = "dir"
	uiCd  = "cd"
	uiPwd = "pwd"
	uiQuit = "quit"
)

const (
	DIR = "DIR"
	CD = "CD"
	PWD = "PWD"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: ", os.Args[0], "host")
		os.Exit(1)
	}

	host := os.Args[1]

	conn, err := net.Dial("tcp", host + ":1202")
	checkError(err)


}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
