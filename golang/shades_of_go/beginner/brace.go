package main

func main() {
	/*
		var s []int
		s = append(s, []int{1, 2, 3}...)
		fmt.Printf("%+v\n", s)
		fmt.Println("len of s: ", len(s))
		fmt.Println("cap of s: ", cap(s))

		var m map[string]int = make(map[string]int, 2)
		m["one"] = 1
		m["two"] = 2
		m["three"] = 3
		fmt.Printf("%+v\n", m)
		for k, _ := range m {
			fmt.Println(k)
		}

		var st string
		fmt.Println(st)

		x := [3]int{1, 2, 3}
		fmt.Printf("Before: %+v\n", x)
		modArray(x)
		fmt.Printf("After: %+v\n", x)
		modAddrArray(&x)
		fmt.Printf("After and After: %+v\n", x)
		sliceMod(x[:len(x)])
		fmt.Printf("After the slice mod: %+v\n", x)

		x := []string{"hjiang", "heng", "ming"}
		for i, v := range x {
			fmt.Println(i, v)
		}

		var x = 2
		table := make([][]int, x)
		for i := range table {
			table[i] = make([]int, rand.Intn(100)+1)
		}
		fmt.Printf("%+v\n", table)

		var randM [][]int
		var err error
		randM, err = genRandMatrix(3, 5, 100)
		if err != nil {
			log.Println(err)
		}
		fmt.Printf("%+v\n", randM)

		x := map[string]string{"One": "1", "two": "", "three": "c"}

		if v := x["w"]; v == "" {
			fmt.Println("no entry")
		}

		if _, ok := x["two"]; !ok {
			fmt.Println("no entry")
		}

		var str string = "hjiang039"
		var strSlice []rune = []rune(str)
		myUpperString(strSlice)
		fmt.Println(string(strSlice))
		fmt.Println(str)
	*/
}

func GetValueByString(table map[string][]byte, key string) []byte {
	var new_key string = key
	if val, ok := table[new_key]; ok {
		return val
	}
	return nil
}

func GetValueBySlice(table map[string][]byte, key []byte) []byte {
	var new_key = key
	if val, ok := table[string(new_key)]; ok {
		return val
	}
	return nil
}

/*
func modArray(orig [3]int) {
	for i := 0; i < len(orig); i++ {
		orig[i] *= 2
	}
}

func modAddrArray(orig *[3]int) {
	for i := 0; i < len(*orig); i++ {
		(*orig)[i] *= 2
	}
}

func sliceMod(orig []int) {
	for i := 0; i < len(orig); i++ {
		orig[i] *= 2
	}
}

//give h(height), w(width), r(elem range)
//generate a h * w matrix which elements between [1, n)
func genRandMatrix(h, w, r int) ([][]int, error) {
	if h*w <= 0 {
		return nil, fmt.Errorf("can't create matrix[%d][%d]", h, w)
	}
	raw := make([]int, h*w)
	for i := range raw {
		raw[i] = 1 + rand.Intn(r)
	}

	table := make([][]int, h)
	for i := range table {
		table[i] = raw[i*w : (i+1)*w]
	}
	return table, nil
}

func upChar(c rune) rune {
	if c >= 'a' && c <= 'z' {
		return unicode.ToUpper(c)
	}
	return c
}

//upcase a golang string
func myUpperString(str []rune) {
	for i, _ := range str {
		str[i] = upChar(str[i])
	}
}
*/
