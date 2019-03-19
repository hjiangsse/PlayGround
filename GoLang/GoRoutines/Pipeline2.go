package main

import (
	"fmt"
)

func main() {
	naturals := make(chan int)
	squares := make(chan int)

	//counter
	go func() {
		for x := 0; x <= 10000 ; x++ {
			naturals <- x
		}
		close(naturals) //when all numbers sended, close the channel
	}()

	//squarer
	go func() {
		/*
		for {
			x, ok := <- naturals
			if !ok {
				break //channel was closed and drained
			}
			squares <- x * x
		}
        */
		for x := range naturals {
			squares <- x * x
		}
		close(squares)
	}()

	//printer(in main goroutine)
	/*
	for {
		x, ok := <- squares
		if !ok {
			fmt.Println("Finished!")
			break
		}
		fmt.Println(x)
	}
    */
	for x := range squares {
		fmt.Println(x)
	}
}
