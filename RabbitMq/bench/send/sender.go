package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"time"

	"github.com/streadway/amqp"
)

func main() {
	msgnum, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	//dial rabbitmq server
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672")
	failOnError(err, "Failed to connect to RabbitMq")
	defer conn.Close()

	//create a channel, which encapsulates most APIs get things done
	ch, err := conn.Channel()
	failOnError(err, "Failed to create channel")
	defer ch.Close()

	//msgBody := make([]byte, 1024)

	start := time.Now()
	for i := 1; i <= msgnum; i++ {
		//fmt.Println(time.Now().UnixNano())
		stamp := strconv.FormatInt(time.Now().UnixNano(), 10)
		//fmt.Println(stamp)
		//copy(msgBody, []byte(stamp))

		err = ch.Publish(
			"",               //exchange
			"mirrored.queue", //routine key
			false,            //mandatory
			false,            //immediate
			amqp.Publishing{
				ContentType: "text/plain",
				Body:        []byte(stamp),
			})
		failOnError(err, "Failed to publish a message")
		time.Sleep(time.Millisecond * 1)
	}
	elapse := time.Now().Sub(start)
	fmt.Printf("elapsed time: %v, send %v messages!\n", elapse, msgnum)
}

func failOnError(err error, msg string) {
	if err != nil {
		log.Fatalf("%s: %s", msg, err)
	}
}
