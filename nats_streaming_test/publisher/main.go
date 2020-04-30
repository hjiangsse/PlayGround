package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"
	"time"

	"github.com/nats-io/stan.go"
)

func main() {
	sc, err := stan.Connect("test-cluster", "publish-client")
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}
	defer sc.Close()

	stopsending := make(chan int)

	go func() {
		i := 1
		//synchronously publish message to server
		for {
			select {
			case <-stopsending:
				return
			default:
			}

			msg := fmt.Sprintf("this is the %dth ruster.", i)

			err = sc.Publish("rust", []byte(msg))
			if err != nil {
				log.Fatal(err)
				os.Exit(1)
			}

			time.Sleep(time.Millisecond * 1000)
			fmt.Printf("send the %vth message.\n", i)
			i++
		}
	}()

	signalChan := make(chan os.Signal, 1)
	cleanupDone := make(chan bool)
	signal.Notify(signalChan, os.Interrupt)
	go func() {
		for range signalChan {
			fmt.Println("\nReceived an interrupt and closing connection...\n\n")
			sc.Close()
			stopsending <- 1
			cleanupDone <- true
		}
	}()
	<-cleanupDone
}
