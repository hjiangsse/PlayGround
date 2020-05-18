package main

import (
	"fmt"
	"log"
	"strconv"
	"time"

	"github.com/streadway/amqp"
)

const (
	msgnum = 100
)

func main() {
	//dial rabbitmq server
	conn, err := amqp.Dial("amqp://guest:guest@localhost:5672")
	failOnError(err, "Failed to connect to RabbitMq")
	defer conn.Close()

	//create a channel, which encapsulates most APIs get things done
	ch, err := conn.Channel()
	failOnError(err, "Failed to create channel")
	defer ch.Close()

	//declare a queue for us to send to,
	//then publish messages to this queue
<<<<<<< HEAD
	/*
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

	//msgBody := "Hello RabbitMq"
	start := time.Now()
	for i := 1; i <= msgnum; i++ {
		err = ch.Publish(
			"",          //exchange
			"q.example", //routine key
			false,       //mandatory
			false,       //immediate
			amqp.Publishing{
				ContentType: "text/plain",
				Body:        []byte(strconv.FormatInt(time.Now().UnixNano(), 10)),
			})
		failOnError(err, "Failed to publish a message")
		time.Sleep(500 * time.Millisecond)
	}
	end := time.Now()
	elapse := end.Sub(start)
	fmt.Printf("elapsed time: %v, send %v messages", elapse, msgnum)
=======
	//queue has a name, just like "subject" in nats
	//or "topic" in nsqd
	q, err := ch.QueueDeclare(
		"hello", //name
		false,   //durale
		false,   //delete when unused
		false,   //exclusive
		false,   //no wait
		nil,     //arguments
	)
	failOnError(err, "Failed to declare a queue")

	msgBody := "Hello RabbitMq"
	err = ch.Publish(
		"",     //exchange
		q.Name, //routine key
		false,  //mandatory
		false,  //immediate
		amqp.Publishing{
			ContentType: "text/plain",
			Body:        []byte(msgBody),
		})
	failOnError(err, "Failed to publish a message")
>>>>>>> ed9876371f1184a34242642d3e3c498ca77d08b8
}

func failOnError(err error, msg string) {
	if err != nil {
		log.Fatalf("%s: %s", msg, err)
	}
}
