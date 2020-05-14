package main

import (
	"fmt"
	"net"
	"os"
)

func main() {
	port := os.Args[1]
	addr := "localhost:" + port

	ln, err := net.Listen("tcp", addr)
	if err != nil {
		panic(err)
	}

	for {
		conn, err := ln.Accept()
		if err != nil {
			panic(err)
		}

		go handleRequest(conn, addr)
	}
}

func handleRequest(conn net.Conn, addr string) {
	buf := make([]byte, 1024)
	reqLen, err := conn.Read(buf)
	if err != nil {
		panic(err)
	}
	fmt.Println("message received: ", string(buf[:reqLen]))

	conn.Write([]byte("message received, and server addr is " + addr))
	conn.Close()
}
