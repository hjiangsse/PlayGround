package main

import (
	"fmt"
	"reflect"
)

/*
type I interface {
	f1(name string)
	f2(name string) (error, float32)
	f3() int64
}

type T int64

func (T) f1(name string) {
	fmt.Println(name)
}

func (T) f2(name string) (error, float32) {
	return nil, 10.2
}

func (T) f3() int64 {
	return 10
}

func TestInter(i I) {
	i.f1("hjiang")
	_, f := i.f2("heng")
	fmt.Println(f)
	d := i.f3()
	fmt.Println(d)
}
*/

// one type implement two interfaces
/*
type I1 interface {
	M1()
}

type I2 interface {
	M2()
}

type T struct{}

func (T) M1() { fmt.Println("T.M1") }
func (T) M2() { fmt.Println("T.M2") }

func f1(i I1) { i.M1() }
func f2(i I2) { i.M2() }
*/

/*
type I interface {
	M()
}

type T1 struct{}

func (T1) M() { fmt.Println("T1.M()") }

type T2 struct{}

func (T2) M() { fmt.Println("T2.M()") }

func f(i I) { i.M() }

type I interface {
	method1()
}

type T struct{}

func (T) method1() {}

type I interface {
	M()
}

type T1 struct{}

func (T1) M() {}

type T2 struct{}

func (T2) M() {}
*/

func main() {
	var i I
	//fmt.Printf("First type of i is %T\n", i)
	//fmt.Printf("First type of i is %s\n", reflect.TypeOf(i).Name())
    i = T1{}
	fmt.Printf("First type of i is %s\n", reflect.TypeOf(i).String())
	//fmt.Printf("Second type of i is %T\n", i)
	i = T2{}
	fmt.Printf("Third type of i is %T\n", i)
}
