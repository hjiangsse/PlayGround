package escape

/*
type TestStruct struct {
	a, b, c, d int
}

func NewTestStruct() *TestStruct {
	return &TestStruct{
		a: 1,
		b: 2,
		c: 3,
		d: 4,
	}
}

func StackNewStruct() TestStruct {
	return TestStruct{
		a: 1,
		b: 2,
		c: 3,
		d: 4,
	}
}

func Sum() int {
	numbers := make([]int, 10)
	return 10
}

func main() {
	answer := Sum()
	fmt.Println(answer)
}
*/

func test(x uint32) bool {
	if x < 5 {
		if x < 10 {
			return true
		}
		panic("x great than 10")
	}
	return false
}
