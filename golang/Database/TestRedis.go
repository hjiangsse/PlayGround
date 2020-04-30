package main

import (
	"fmt"
	"os"
	"time"
	"github.com/go-redis/redis"
)

type User struct {
	Age int
	Name string
	Text string
}

func main() {

	user_record := User{23, "hjiang", "He is a student from anhui normal university, and now he is working in shanghai stock exchange."}

	start := time.Now()
	insert_record_ntimes("localhost:6379", "", "testdb", &user_record, 1000000)
	elapsed := time.Since(start)
	fmt.Printf("%s took %s\n", elapsed)
    /*
	redisdb := redis.NewClient(&redis.Options{
		Addr: "localhost:6379", //default address
		Password: "",           //no passwd set
		DB:       0,            //use default db
	})

	pong, err := redisdb.Ping().Result()
	fmt.Println(pong, err)

	err = redisdb.Set("key", "value", 0).Err()
	checkError(err)

	//fmt.Println(redisdb.ClientGetName())

	field1 := map[string]interface{}{
		"Name" : "Hjiang",
		"Age"  : "23",
		"Address" : "Pudong Shanghai",
	}

	err = redisdb.HMSet("students", field1).Err()
	checkError(err)
    */

	os.Exit(0)
}

func insert_record_ntimes(db_address, db_passwd, db_name string, record *User, times uint64) {

	options := redis.Options{
		Addr: db_address,
		Password: db_passwd,
		DB: 0,
	}

	redisdb := redis.NewClient(&options)

	pong, err := redisdb.Ping().Result()
	fmt.Println(pong, err)

	rcd_str := fmt.Sprintf("%d|%s|%s", record.Age, record.Name, record.Text)

	for i := uint64(0); i < times; i++ {
		index := fmt.Sprintf("%d", i)
		field := map[string]interface{}{
			index : rcd_str,
		}

		err = redisdb.HMSet(db_name, field).Err()
		checkError(err)
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
