package main

import (
	"fmt"
	"os"

	"github.com/pkg/errors"
)

func main() {
	_, err := open_file("./bad.txt")
	if err != nil {
		fmt.Println("full error: ")
		fmt.Println(err.Error())
		fmt.Println("inner error: ")
		innererr := errors.Cause(err)
		if innererr != nil {
			fmt.Println(innererr.Error())
		}
	}
}

func open_file(filepath string) (*os.File, error) {
	f, err := os.Open(filepath)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("open %s file failed!", filepath))
	}
	return f, nil
}
