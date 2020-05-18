package main

import (
	"fmt"
	"log"
	"math/rand"
	"os"
	"strings"
	"time"

	"github.com/streadway/amqp"
)

var characters = []string{"strive", "mediocrity", "lazy"}
var colors = []string{"red", "green", "orange"}
var animals = []string{"rabbit", "tiger", "duck"}

func main() {
	//dial rabbitmq server
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672/")
	failOnError(err, "Failed to connect to RabbitMq")
	defer conn.Close()

	//create a channel
	ch, err := conn.Channel()
	failOnError(err, "Failed to create channel")
	defer ch.Close()

	err = ch.ExchangeDeclare(
		"animal-checking", // nane
		"topic",           // type
		true,              // durable
		false,             // auto-deleted
		false,             // internal
		false,             // no-wait
		nil,               // arguments
	)
	failOnError(err, "Failed to declear an exchange")

	msgBody := bodyFrom(os.Args)
	for {
		rtkey := genRouteKey()
		fmt.Println(rtkey)

		err = ch.Publish(
			"animal-checking", //exchange
			rtkey,             //routine key
			false,             //mandatory
			false,             //immediate
			amqp.Publishing{
				DeliveryMode: amqp.Persistent,
				ContentType:  "text/plain",
				Body:         []byte(msgBody + "[" + rtkey + "]"),
			})
		failOnError(err, "Failed to publish a message")

		time.Sleep(time.Second)
	}
}

func genRouteKey() string {
	chaIdx, corIdx, aniIdx := rand.Intn(3), rand.Intn(3), rand.Intn(3)
	return characters[chaIdx] + "." + colors[corIdx] + "." + animals[aniIdx]
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
