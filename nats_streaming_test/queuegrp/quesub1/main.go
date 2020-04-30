package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"

	"github.com/nats-io/nats.go"
	"github.com/nats-io/stan.go"
)

func main() {
	nc, err := nats.Connect("nats://localhost:4223")
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	sc, err := stan.Connect("test-cluster",
		"quesub1",
		stan.NatsConn(nc),
		stan.Pings(10, 5),
		stan.SetConnectionLostHandler(func(_ stan.Conn, reason error) {
			log.Fatalf("Connection lost, reason: %v", reason)
		}))
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	//create a queue subscriber on "rust" for group "hacker"
	qsub, err := sc.QueueSubscribe("rust", "hacker", func(msg *stan.Msg) {
		fmt.Printf("[quesub1] recv message: %v, seq: %v\n", string(msg.Data), msg.Sequence)
	}, stan.DeliverAllAvailable())
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	signalChan := make(chan os.Signal, 1)
	cleanupDone := make(chan bool)
	signal.Notify(signalChan, os.Interrupt)
	go func() {
		for range signalChan {
			fmt.Println("\nReceived an interrupt and closing connection...\n\n")
			qsub.Unsubscribe()
			sc.Close()
			cleanupDone <- true
		}
	}()
	<-cleanupDone
}
