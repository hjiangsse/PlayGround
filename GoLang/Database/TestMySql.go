package main

import (
	"fmt"
	"os"
	"time"
	"database/sql"
	_ "github.com/go-sql-driver/mysql"
)

type Tag struct {
	ID int
	NAME string
}

type User struct {
	Age int
	Name string
	Text string
}

func insert_record_ntimes(db_type, db_user, db_pass, db_addr, db_port string, record *User, db_name, tb_name string, times uint64) {
	conn_str := db_user + ":" + db_pass + "@tcp(" + db_addr + ":" + db_port + ")/" + db_name

	db, err := sql.Open(db_type, conn_str)
	checkError(err)

	_ = db

	insert_cmd := fmt.Sprintf("INSERT INTO %s(age, name) VALUES(%d, '%s');",tb_name, record.Age, record.Name);

	for i := uint64(0); i < times; i++ {
		insert, err := db.Query(insert_cmd)
		checkError(err)

		insert.Close()
	}
}

func write_to_file_ntimes(file_name string, record *User, times uint64) {
	//file already exist, then delete it
	if _, err := os.Stat(file_name); !os.IsNotExist(err) {
		err = os.Remove(file_name)
		checkError(err)
	}

	hndl, err := os.OpenFile(file_name, os.O_RDWR | os.O_CREATE, 0666)
	checkError(err)

	for i := uint64(0); i < times; i++ {
		write_str := fmt.Sprintf("%d|%s|%s\n", record.Age, record.Name, record.Text)
		_, err := hndl.WriteString(write_str)
		checkError(err)
	}

	hndl.Close()
}

func main() {
	/*
	fmt.Println("Now let's connect to MySQL database!")

	db, err := sql.Open("mysql", "hjiang:jiang186212@tcp(127.0.0.1:3306)/test")
	checkError(err)

	defer db.Close()
    */

	user_record := User{23, "hjiang", "He is a student from anhui normal university, and now he is working in shanghai stock exchange."}
	/*
	start := time.Now()
	insert_record_ntimes("mysql", "hjiang", "jiang186212", "localhost", "3306", &user_record, "test", "user", 10000);
	elapsed := time.Since(start)
	fmt.Printf("%s took %s\n", elapsed)
    */

	start := time.Now()
	write_to_file_ntimes("test.txt", &user_record, 1000000)
	elapsed := time.Since(start)
	fmt.Printf("%s took %s\n", elapsed)
	/*
	tb_show, err := db.Query("SHOW TABLES;")
	checkError(err)

	fmt.Println(tb_show)
    */

	/* test insert
	insert, err := db.Query("INSERT INTO corporation VALUES ( 23, 'hjiang cop' )")
	checkError(err)

	defer insert.Close()

	results, err := db.Query("SELECT corp_id, name FROM corporation;")
	checkError(err)

	for results.Next() {
		var tag Tag

		err = results.Scan(&tag.ID, &tag.NAME)
		checkError(err)

		fmt.Println(tag)
	}
    */

	os.Exit(0)
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
