package main

import (
	"os"
	"runtime/trace"
)

func main() {
	trace.Start(os.Stdout)
	defer trace.Stop()

	ch := make(chan string)
	go func() {
		ch <- "HJIANG"
	}()

	<-ch
}
