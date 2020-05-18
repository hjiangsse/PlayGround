package main

import (
	"log"
	"math/rand"
	"os"
	"strings"
	"time"

	"github.com/streadway/amqp"
)

func main() {
	routineKeys := []string{"red", "green", "blue"}

	//dial rabbitmq server
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672/")
	failOnError(err, "Failed to connect to RabbitMq")
	defer conn.Close()

	//create a channel
	ch, err := conn.Channel()
	failOnError(err, "Failed to create channel")
	defer ch.Close()

	err = ch.ExchangeDeclare(
		"balls",  // name
		"direct", // this is a direct exchange
		true,     // durable
		false,    // auto-deleted
		false,    // internal
		false,    // no-wait
		nil,      // arguments
	)
	failOnError(err, "Failed to declear an exchange")

	msgBody := bodyFrom(os.Args)
	for {
		keyIdx := rand.Intn(3)

		err = ch.Publish(
			"balls",             //exchange
			routineKeys[keyIdx], //routine key
			false,               //mandatory
			false,               //immediate
			amqp.Publishing{
				DeliveryMode: amqp.Persistent,
				ContentType:  "text/plain",
				Body:         []byte(msgBody + "[" + routineKeys[keyIdx] + "]"),
			})
		failOnError(err, "Failed to publish a message")

		time.Sleep(time.Second)
	}
}

func bodyFrom(args []string) string {
	var s string
	if (len(args) < 2) || os.Args[1] == "" {
		s = "hello..."
	} else {
		s = strings.Join(args[1:], " ")
	}
	return s
}

func failOnError(err error, msg string) {
	if err != nil {
		log.Fatalf("%s: %s", msg, err)
	}
}
