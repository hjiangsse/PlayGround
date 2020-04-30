package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"

	"github.com/nats-io/stan.go"
)

const (
	wanted = 5
)

func main() {
	sc, err := stan.Connect("test-cluster", "subcriber-client")
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	stopsub := make(chan int)
	go func() {
		_, err := sc.Subscribe("rust", func(m *stan.Msg) {
			fmt.Printf("[First]Received a message: %s, seq: %v\n", string(m.Data), m.Sequence)
			if m.Sequence >= 40 {
				m.Ack()
				sc.Close()
				stopsub <- 1
			}
			m.Ack() //manully send ack back to server
		}, stan.StartAtSequence(1), stan.SetManualAckMode(), stan.DurableName("my-durename"))

		if err != nil {
			log.Fatal(err)
			os.Exit(1)
		}
	}()

	<-stopsub
	fmt.Println("stop the first subcribe, and close the connection")

	sc, err = stan.Connect("test-cluster", "subcriber-client")
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	sc.Subscribe("rust", func(m *stan.Msg) {
		fmt.Printf("[Second]Received a message: %s, seq: %v\n", string(m.Data), m.Sequence)
	}, stan.DurableName("my-durename"))

	signalChan := make(chan os.Signal, 1)
	cleanupDone := make(chan bool)
	signal.Notify(signalChan, os.Interrupt)
	go func() {
		for range signalChan {
			fmt.Println("\nReceived an interrupt and closing connection...\n\n")
			//sub.Unsubscribe()
			sc.Close()
			cleanupDone <- true
		}
	}()
	<-cleanupDone
}
