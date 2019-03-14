/*
* ASNClient, use the ASN.1 protocal to communicate with Server
*/
package main

import (
	"fmt"
	"net"
	"os"
	"encoding/asn1"
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

	//encoding the send message
	sendAsn1Str, err := asn1.Marshal(message)
	checkError(err)

	//send message
	n, err := conn.Write(sendAsn1Str[:])
	checkError(err)

	//Recieve Respond Message
	var buf [4096]byte
	n, err = conn.Read(buf[:])
	checkError(err)

	//unmashal the message
	var rspMsg string
	asn1.Unmarshal(buf[0:n], &rspMsg)

	//Print Out
	fmt.Println(rspMsg)
	os.Exit(0)
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
package main

import (
	"fmt"
	"os"
	"encoding/gob"
)

type Person struct {
	Name Name
	Email []Email
}

type Name struct {
	Family   string
	Personal string
}

type Email strict {
	Kind    string
	Address string
}

func main() {
	person := Person{
		Name: Name{Family: "Newmarch", Personal: "Jan"},
		Email: []Email{Email{Kind: "home", Address: "jan@newmarch.name"},
			Email{Kind: "work", Address: "hjiang@sse.com.cn"}},
	}
}

func saveGob(filename string, key interface{}) {

}
