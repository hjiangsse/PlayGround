package benching

func Fib2(n int) int {
	switch n {
	case 0:
		return 0
	case 1:
		return 1
	default:
		return Fib2(n-1) + Fib2(n-2)
	}
}

func Fib3(n int) int {
	switch n {
	case 0:
		return 0
	case 1:
		return 1
	case 2:
		return 1
	default:
		return Fib2(n-1) + Fib2(n-2)
	}
}

func Fib_Iter(n int) int {
	a := 0
	b := 1
	c := 0

	if n == 0 {
		return a
	}

	if n == 1 {
		return b
	}

	//fmt.Println("------------------------------------------")
	//fmt.Printf("n = %d\n", n)
	for i := 2; i <= n; i++ {
		c = b + a
		a = b
		b = c

		//fmt.Printf("a = %d, b = %d, c = %d\n", a, b, c)
	}
	//fmt.Println("------------------------------------------")

	return c
}
