package main

import (
	"fmt"
	"bytes"
	"os"
)

func main() {
	proverbs := []string{
		"This is the 1st line.",
		"This is the 2st line.",
		"This is the 3st line.",
	}

	var writer bytes.Buffer

	for _, p := range proverbs {
		n, err := writer.Write([]byte(p))
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		if n != len(p) {
			fmt.Println("failed to write data")
			os.Exit(1)
		}
	}

	fmt.Println(writer.String())
}
