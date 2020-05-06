package main

import (
	"log"
	"os"
	"strings"

	"github.com/streadway/amqp"
)

func main() {
	//dial rabbitmq server
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672")
	failOnError(err, "Failed to connect to RabbitMq")
	defer conn.Close()

	//create a channel
	ch, err := conn.Channel()
	failOnError(err, "Failed to create channel")
	defer ch.Close()

	//declare a queue for us to send to,
	//then publish messages to this queue
	q, err := ch.QueueDeclare(
		"hello", //name
		false,   //durale
		false,   //delete when unused
		false,   //exclusive
		false,   //no wait
		nil,     //arguments
	)
	failOnError(err, "Failed to declare a queue")

	msgBody := bodyFrom(os.Args)
	err = ch.Publish(
		"",     //exchange
		q.Name, //routine key
		false,  //mandatory
		false,  //immediate
		amqp.Publishing{
			DeliveryMode: amqp.Persistent,
			ContentType:  "text/plain",
			Body:         []byte(msgBody),
		})
	failOnError(err, "Failed to publish a message")
}

func bodyFrom(args []string) string {
	var s string
	if (len(args) < 2) || os.Args[1] == "" {
		s = "hello"
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
