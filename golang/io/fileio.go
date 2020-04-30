package main

import (
	"os"
	"bytes"
	"fmt"
	"io"
	"bufio"
)

func main() {
	/* write some slice of strings into a file */
	/*
	sentences := []string{
		"This is the end of the china.\n",
		"This is the end of the world.\n",
		"This is our china max number.\n",
	}

	file, err := os.Create("./TestFile.txt")
	checkError(err)

	defer file.Close()

	for _, p := range sentences {
		n, err := file.Write([]byte(p))
		checkError(err)

		if n != len(p) {
			fmt.Println("Failed to write data")
			os.Exit(1)
		}
	}

	fmt.Println("File Write Done!")

	file, err := os.Open("./TestFile.txt")
	checkError(err)

	buf := make([]byte, 1024)
	for {
		n, err := file.Read(buf[:])
		if err == io.EOF {
			break
		}
		fmt.Print(string(buf[:n]))
	}

	sentences := []string{
		"This is the end of the china.\n",
		"This is the end of the world.\n",
		"This is our china max number.\n",
	}

	for _, p := range sentences {
		n, err := os.Stdout.Write([]byte(p))
		checkError(err)

		if n != len(p) {
			fmt.Println("Failed to write data")
			os.Exit(1)
		}
	}

	sentences := new(bytes.Buffer)
	sentences.WriteString("This is the first line.\n")
	sentences.WriteString("This is the second line.\n")
	sentences.WriteString("This is the third line.\n")

	//fmt.Println(sentences.String())
	file, err := os.Create("./sentences.txt")
	checkError(err)

	defer file.Close()

	_, err = io.Copy(file, sentences)
	checkError(err)

	fmt.Println("File Created!")

	file, err := os.Open("./sentences.txt")
	checkError(err)

	defer file.Close()

	_ , err = io.Copy(os.Stdout, file)
	checkError(err)

	file, err := os.Create("magic_string.txt")
	checkError(err)

	defer file.Close()

	_, err = io.WriteString(file, "This is a kind of magic!\n")
	checkError(err)
    */

	//pipe writers and readers
	/*
	sentences := new(bytes.Buffer)
	sentences.WriteString("This is the first line.\n")
	sentences.WriteString("This is the second line.\n")
	sentences.WriteString("This is the third line.\n")

	piper, pipew := io.Pipe()

	//write in the write end of the pipe
	go func() {
		defer pipew.Close()
		io.Copy(pipew, sentences)
	}()

	//read in the read end of the pipe
	io.Copy(os.Stdout, piper)
	piper.Close()
    */
}

func checkError(err error) {
	if err != nil {
		fmt.Println(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
