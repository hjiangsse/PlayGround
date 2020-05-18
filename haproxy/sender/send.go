package main

import (
	"fmt"
	"net"
	"strconv"
)

func main() {
	//connect to haproxy front-end
	for i := 0; i < 100; i++ {
		conn, err := net.Dial("tcp", "localhost:9123")
		if err != nil {
			panic(err)
		}

		_, err = conn.Write([]byte("message from client" + strconv.Itoa(i+1)))
		if err != nil {
			panic(err)
		}

		reply := make([]byte, 1024)

		recvLen, err := conn.Read(reply)
		if err != nil {
			panic(err)
		}
		fmt.Println("response from server: ", string(reply[:recvLen]))
		conn.Close()
	}
}
