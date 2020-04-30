package main

import (
	"fmt"
)

type echoFunc func(speak string) string
type speakMap map[string]echoFunc

func main() {
	myecho := make(speakMap)
	myecho["low_speak"] = low_speak
	myecho["loud_speak"] = loud_speak

	fmt.Println(myecho)

	fun1 := myecho["low_speak"]
	fmt.Println(fun1("This is a low speak"))
}

func low_speak(speak string) string {
	ret := fmt.Sprintf("Low: %s",speak);
	return ret
}

func loud_speak(speak string) string {
	ret := fmt.Sprintf("Loud: %s", speak)
	return ret
}
