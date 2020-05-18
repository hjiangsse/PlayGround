package main

import (
	"log"
	"os"

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

	q, err := ch.QueueDeclare(
		"",    //empty name
		false, //durale
		false, //delete when unused
		true,  //exclusive
		false, //no wait
		nil,   //arguments
	)
	failOnError(err, "Failed to declare a queue")

	if len(os.Args) < 2 {
		log.Printf("Usage: %s [binding_key]...", os.Args[0])
		os.Exit(0)
	}

	//bind queue to a exchange
	for _, routinekey := range os.Args[1:] {
		log.Printf("Binding queue %s to exchange %s with routing key %s",
			q.Name, "animal-checking", routinekey)

		err = ch.QueueBind(
			q.Name,
			routinekey,
			"animal-checking",
			false,
			nil,
		)
		failOnError(err, "Failed to bind the queue to exchange")
	}

	msgs, err := ch.Consume(
		q.Name,
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
			log.Printf("Recived a message: %s\n", d.Body)
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