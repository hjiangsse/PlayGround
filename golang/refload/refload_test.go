package refload

import (
	"fmt"
	"reflect"
	"testing"
)

//convString test1: normal test
func Test_convString_int(t *testing.T) {
	//[int] test
	var x int = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_int8(t *testing.T) {
	//[int] test
	var x int8 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_int16(t *testing.T) {
	//[int] test
	var x int16 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_int32(t *testing.T) {
	//[int] test
	var x int32 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_int64(t *testing.T) {
	//[int] test
	var x int64 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_uint(t *testing.T) {
	//[int] test
	var x uint = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("900000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_uint8(t *testing.T) {
	//[int] test
	var x uint8 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_uint16(t *testing.T) {
	//[int] test
	var x uint16 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_uint32(t *testing.T) {
	//[int] test
	var x uint32 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_uint64(t *testing.T) {
	//[int] test
	var x uint64 = 10
	if v, err := convString("100", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == 100 {
		t.Log("conv [string] to [int] ok! test pass!")
	}

	if v, err := convString("100000000000000000", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else {
		fmt.Println(v)
	}
}

func Test_convString_string(t *testing.T) {
	//[int] test
	var x string = "hjiang"
	if v, err := convString("heng", reflect.TypeOf(x)); err != nil {
		t.Errorf("test conv [string] to [int] fail: %s", err)
	} else if v == "heng" {
		t.Log("conv [string] to [int] ok! test pass!")
	}
}

//field of struct is valid, line in file valid
func Test_LoadRefDat_1(t *testing.T) {
	type testElem struct {
		Field1 int
		Field2 uint
		Field3 string
	}

	var elemSlice []testElem
	if err := LoadRefDat("./testfile/testfile1.dat", "|", &elemSlice); err != nil {
		t.Errorf("valid struct, valid file, but: %s", err)
	} else {
		fmt.Println(elemSlice)
	}
}

//field of struct is valid, line in file invalid
//"0123T" conv to int err?
func Test_LoadRefDat_2(t *testing.T) {
	type testElem struct {
		Field1 int
		Field2 uint
		Field3 string
	}

	var elemSlice []testElem
	if err := LoadRefDat("./testfile/testfile2.dat", "|", &elemSlice); err != nil {
		str := fmt.Sprintf("valid struct, valid file, but: %s", err)
		t.Log(str)
	} else {
		fmt.Println(elemSlice)
	}
}

//field of struct is valid, line in file invalid
//line segs num less than struct field number
func Test_LoadRefDat_3(t *testing.T) {
	type testElem struct {
		Field1 int
		Field2 uint
		Field3 string
	}

	var elemSlice []testElem
	if err := LoadRefDat("./testfile/testfile3.dat", "|", &elemSlice); err != nil {
		str := fmt.Sprintf("valid struct, invalid file, %s", err)
		t.Log(str)
	} else {
		fmt.Println(elemSlice)
	}
}
