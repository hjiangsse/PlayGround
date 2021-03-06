package main

import (
	"log"
	"strconv"
	"time"

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

	/*
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
	*/

	msgs, err := ch.Consume(
		"q.example",
		"",
		true,  //auto ack
		false, //exclusive
		false, //no-local
		false, //no-wait
		nil,   //args
	)
	failOnError(err, "Can not register a consumer")

	forever := make(chan bool)

	go func() {
		for d := range msgs {
			send, _ := strconv.ParseInt(string(d.Body), 10, 64)
			recv := time.Now().UnixNano()
			eltime := recv - send

			log.Printf("Recived a message time elapsed: %v\n", eltime)
		}
	}()

	log.Printf(" [*]Waiting for message, To exit press Ctrl+C")
	<-forever
}

func failOnError(err error, msg string) {
	if err != nil {
		log.Fatalf("%s: %s", msg, err)
	}
}
