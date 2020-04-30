package main

import(
	"fmt"
	"io"
	"strings"
)

func main() {
	reader := strings.NewReader("This is the end of the world.")
	p := make([]byte, 4)

	for {
		n, err := reader.Read(p)
		if err == io.EOF {
			break
		}
		fmt.Println(string(p[:n]))
	}
}
