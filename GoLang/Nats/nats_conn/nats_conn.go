package main

import (
	"log"
	"time"

	"github.com/nats-io/nats.go"
)

func main() {
	//nc, err := nats.Connect(nats.DefaultURL)
	//nc, err := nats.Connect("demo.nats.io")
	/*
		//a connection with a name and a timeout
		nc, err := nats.Connect(nats.DefaultURL, nats.Name("This is a example"), nats.Timeout(1*time.Second))
	*/
	//nc, err := nats.Connect("nats://127.0.0.1:4222", nats.Name("This is a example"), nats.PingInterval(20*time.Second), nats.MaxPingsOutstanding(5))
	/*
		//deal disconnect and reconnect
		nc, err := nats.Connect(nats.DefaultURL,
			nats.DisconnectErrHandler(func(nc *nats.Conn, err error) {
				// handle disconnect error event
				log.Println("disconnect happen!")
			}),
			nats.ReconnectHandler(func(nc *nats.Conn) {
				// handle reconnect event
				log.Println("reconnect happen!")
			}))
	*/
	nc, err := nats.Connect(nats.DefaultURL, nats.Name("This is a example"))
	if err != nil {
		log.Fatal(err)
	}
	defer nc.Close()

	for i := 0; i < 10; i++ {
		if err := nc.Publish("updates", []byte("This is a new world")); err != nil {
			log.Fatal(err)
		}
		time.Sleep(time.Second)
	}
}
