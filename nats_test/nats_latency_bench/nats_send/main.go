package main

import (
	"encoding/binary"
	"log"
	"os"
	"strconv"
	"time"

	"github.com/nats-io/nats.go"
)

func main() {
	if len(os.Args) <= 2 {
		log.Fatal("argument not enough")
		os.Exit(1)
	}

	msgnum, err := strconv.Atoi(os.Args[1])
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	bodysize, err := strconv.Atoi(os.Args[2])
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	if bodysize < 8 {
		log.Fatal("msgbody size is not enough")
		os.Exit(1)
	}

	nc, err := nats.Connect("nats://127.0.0.1:4222")
	if err != nil {
		log.Fatal(err)
	}
	defer nc.Close()

	msgbody := make([]byte, bodysize)

	for i := 0; i < msgnum; i++ {
		binary.LittleEndian.PutUint64(msgbody, uint64(time.Now().UnixNano()))

		//send the request
		if err := nc.Publish("updates", msgbody); err != nil {
			log.Fatal(err)
			os.Exit(1)
		}

		time.Sleep(time.Microsecond * 1)
	}
}
