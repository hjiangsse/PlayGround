package main

import (
	"encoding/binary"
	"log"
	"os"
	"strconv"
	"time"

	"github.com/nats-io/nats.go"
	"gonum.org/v1/gonum/stat"
)

func main() {
	if len(os.Args) <= 1 {
		log.Fatal("argument not enough")
		os.Exit(1)
	}

	msgnum, err := strconv.Atoi(os.Args[1])
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	nc, err := nats.Connect("nats://127.0.0.1:4222")
	if err != nil {
		log.Fatal(err)
	}
	defer nc.Close()

	sub, err := nc.SubscribeSync("updates")
	if err != nil {
		log.Fatal(err)
	}
	defer sub.Unsubscribe()

	lantencys := make([]float64, msgnum)
	for i := 0; i < msgnum; i++ {
		msg, err := sub.NextMsg(10 * time.Second)
		if err != nil {
			log.Fatal(err)
		}
		sendtime := binary.LittleEndian.Uint64(msg.Data[0:8])
		lantency := uint64(time.Now().UnixNano()) - sendtime
		lantencys[i] = float64(lantency)
	}

	mean := stat.Mean(lantencys, nil)
	stddev := stat.StdDev(lantencys, nil)

	log.Printf("mean latency of %v messages is %v\n", msgnum, mean)
	log.Printf("stddev of latencies of %v messages is %v\n", msgnum, stddev)
}
