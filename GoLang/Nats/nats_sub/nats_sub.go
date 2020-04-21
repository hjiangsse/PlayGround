package main

import (
	"log"
	"time"

	"github.com/nats-io/nats.go"
)

func main() {
	nc, err := nats.Connect(nats.DefaultURL)
	if err != nil {
		log.Fatal(err)
	}
	defer nc.Close()

	// Subscribe synchrolly
	sub, err := nc.SubscribeSync("updates")
	if err != nil {
		log.Fatal(err)
	}

	// Wait for the first message
	msg, err := sub.NextMsg(10 * time.Second)
	if err != nil {
		log.Fatal(err)
	}
	log.Printf("First Reply: %s\n", msg.Data)

	msg, err = sub.NextMsg(10 * time.Second)
	if err != nil {
		log.Fatal(err)
	}
	log.Printf("Second Reply: %s\n", msg.Data)

	//after receive the second message, unsub
	if err := sub.Unsubscribe(); err != nil {
		log.Fatal(err)
	}
	/*
		wg := sync.WaitGroup{}

		wg.Add(1)
		if _, err := nc.Subscribe("updates", func(m *nats.Msg) {
			log.Println(string(m.Data))
			wg.Done()
		}); err != nil {
			log.Fatal(err)
		}
	*/

	//wg.Wait()
}
