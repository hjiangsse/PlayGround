package main

import (
	"fmt"
	"os"
	"github.com/go-redis/redis"
)

func main() {
	addr := "localhost:6379"
	pass := ""

	redisdb := GetRedisConn(addr, pass)
	if redisdb  == nil {
		fmt.Println("Invalid connection!")
	}

	SetRedisHash(redisdb, "students", "name", "hjiang")

	os.Exit(0)
}

/*
* Get a redis connection
* Parameter:
*     db_addr : redis address, such as localhost:6379
*     db_pass : redis passwd, empty ""
*/
func GetRedisConn(db_addr, db_pass string) *redis.Client {
	//Create a new client
	redisdb := redis.NewClient(&redis.Options{
		Addr: db_addr,
		Password: db_pass,
		DB: 0, //use default db
	})

	//test if this connection is valid
	pong, err := redisdb.Ping().Result()
	checkError(err)

	if pong == "PONG" {
		return redisdb
	}

	return nil
}

/*
* Set Redis Hash Table(key and value)
* Parameter:
*     redisDb : redis database connection
*	  db_name : the hash table you want to mofify
*     key     : key
*     value   : value
*/
func SetRedisHash(redisDb *redis.Client, db_name, key, value string) {
	field := map[string]interface{}{
		key : value,
	}

	err := redisDb.HMSet(db_name, field).Err()
	checkError(err)
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
