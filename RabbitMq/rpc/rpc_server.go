package main

import (
	"log"
	"strconv"

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

	q, err := ch.QueueDeclare(
		"rpc_queue", //empty name
		false,       //durale
		false,       //delete when unused
		false,       //exclusive
		false,       //no wait
		nil,         //arguments
	)
	failOnError(err, "Failed to declare a queue")

	err = ch.Qos(
		1, //prefetch count
		0, //prefetch size
		false,
	)

	msgs, err := ch.Consume(
		q.Name,
		"",
		false, //auto ack
		false, //exclusive
		false, //no-local
		false, //no-wait
		nil,   //args
	)
	failOnError(err, "Can not register a consumer")

	forever := make(chan bool)

	go func() {
		for d := range msgs {
			//log.Printf("Recived a message: %s\n", d.Body)
			n, err := strconv.Atoi(string(d.Body))
			failOnError(err, "Failed to convert body to integer")

			log.Printf(" [.] fib(%d)", n)
			response := fib(n)

			err = ch.Publish(
				"",        //use the default exchange
				d.ReplyTo, //routing key
				false,
				false,
				amqp.Publishing{
					ContentType:   "text/plain",
					CorrelationId: d.CorrelationId,
					Body:          []byte(strconv.Itoa(response)),
				},
			)
			failOnError(err, "Failed to publish a message")

			d.Ack(false)
		}
	}()

	log.Printf(" [*]Waiting for message, To exit press Ctrl+C")
	<-forever
}

func fib(n int) int {
	if n == 0 {
		return 0
	} else if n == 1 {
		return 1
	} else {
		return fib(n-1) + fib(n-2)
	}
}

func failOnError(err error, msg string) {
	if err != nil {
		log.Fatalf("%s: %s", msg, err)
	}
}
