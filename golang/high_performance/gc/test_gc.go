package main

import (
	"fmt"
	"net/http"
	_ "net/http/pprof"
)

func main() {
	ip := "0.0.0.0:6061"
	if err := http.ListenAndServe(ip, nil); err != nil {
		fmt.Println("start pprof failed on %s\n", ip)
	}
}
