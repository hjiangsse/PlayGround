/*
* Echo Client, Send something to Server and recieve
* the same thing from Server
*/
package main

import (
	"fmt"
	"net"
	"os"
)

func main() {
	if len(os.Args) != 3 {
		fmt.Fprintf(os.Stderr, "Usage: %s host:port message", os.Args[0])
		os.Exit(1)
	}

	serverAddr := os.Args[1]
	message := os.Args[2]

	tcpAddr, err := net.ResolveTCPAddr("tcp4", serverAddr)
	checkError(err)

	conn, err := net.DialTCP("tcp", nil, tcpAddr)
	checkError(err)

	n, err := conn.Write([]byte(message))
	checkError(err)

	var buf [512]byte
	n, err = conn.Read(buf[:])
	checkError(err)

	fmt.Println(string(buf[0:n]))
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
