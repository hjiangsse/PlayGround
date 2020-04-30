package main

import (
	"fmt"
)

/*
type T1 struct {
	name string
}

type T2 struct {
	name string
}

type T struct {
	name string
}

type I1 interface {
	M1()
}

type I2 interface {
	M1()
}

type T struct {}

func (T) M1() {}
*/

type I1 interface {
	M1()
}

type T struct{}

func (T) M1() {}

func main() {
	var v1 I1 = T{}
	var v2 T = v1
}
