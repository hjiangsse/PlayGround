/*
* ASNServer, use the ASN.1 protocal to communicate with Client
*/
package main

import (
	"fmt"
	"net"
	"os"
	"encoding/asn1"
)

func main() {
	service := ":1201"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	checkError(err)

	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err)

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		go handleClient(conn)
	}
}

func handleClient(conn net.Conn) {
	defer conn.Close()

	var buf [4096]byte
	for {
		n, err := conn.Read(buf[:])
		if err != nil {
			return
		}

		//decoding the asn.1 message
		var recieveStr string
		asn1.Unmarshal(buf[0:n], &recieveStr)

		//encoding the respond message
		sendStr := "Respond: " + recieveStr
		sendAsn1Str, err := asn1.Marshal(sendStr)
		checkError(err)

		//send respond message
		_, err = conn.Write(sendAsn1Str[:])
		checkError(err)
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
