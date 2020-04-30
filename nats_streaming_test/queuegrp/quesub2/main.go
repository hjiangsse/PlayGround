package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"

	"github.com/nats-io/stan.go"
)

func main() {
	sc, err := stan.Connect("test-cluster", "quesub2")
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	//create a queue subscriber on "rust" for group "hacker"
	qsub, err := sc.QueueSubscribe("rust", "hacker", func(msg *stan.Msg) {
		fmt.Printf("[quesub2] recv message: %v, seq: %v\n", string(msg.Data), msg.Sequence)
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
