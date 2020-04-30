package main

import (
	"flag"
	"fmt"
	"os"
)

/*
type Student struct {
	Name string
	Age  uint32
}

func (s *Student) String() string {
	return fmt.Sprintf("[name: %v; age %v]", s.Name, s.Age)
}

func (s *Student) Set(str string) error {
	s.Name = str
	s.Age = 20
	return nil
}
*/

type Dog struct {
	age  int
	name string
}

type Cat struct {
	age     int
	name    string
	friends []string
}

func main() {
	/*
		var ip = flag.Int("flagname", 1234, "help message for flagname")

		var flagvar int
		flag.IntVar(&flagvar, "newflag", 1234, "help message for newflag")

		var defaultflag Student
		flag.Var(&defaultflag, "defaultflag", "help message for default flag")

		flag.Parse()
		fmt.Println(*ip)
		fmt.Println(flagvar)
		fmt.Println(defaultflag)

		fmt.Println("print all the argument after the parsing: ")
		fmt.Println(flag.Args())
	*/

	//test flagset
	var fs flag.FlagSet
	var (
		showVersion bool
		showHelp    bool
		showTLSHelp bool
	)

	fs.BoolVar(&showVersion, "v", false, "show server version message")
	fs.BoolVar(&showVersion, "version", false, "show server version message")
	fs.BoolVar(&showHelp, "h", false, "show server help information")
	fs.BoolVar(&showHelp, "help", false, "show server help information")
	fs.BoolVar(&showTLSHelp, "tv", false, "show server TLS help information")
	fs.BoolVar(&showTLSHelp, "tlsversion", false, "show server TLS help information")

	if err := fs.Parse(os.Args[1:]); err != nil {
		panic(err)
	}

	if showVersion {
		fmt.Println("This is the server infomation show")
	}

	if showHelp {
		fmt.Println("This is the server help infomation show")
	}

	if showTLSHelp {
		fmt.Println("This is the TLS help information show")
	}

	fmt.Println("remain args: ", fs.Args())
	fs.Visit(func(f *flag.Flag) {
		switch f.Name {
		case "dv":
			fmt.Println("dv", f.Value)
		case "h":
			fmt.Println("h: ", f.Value)
		case "v":
			fmt.Println("v:", f.Value)
		}
	})
	/*
		//shallow copy and deep copy
		roger := Dog{5, "roger"}
		mydog := roger

		if mydog == roger {
			fmt.Println("roger and mydog are equal structs")
		}

		friendsSlice := []string{"abba", "cccd", "cccp"}
		mimi := Cat{
			8, "mimi", friendsSlice,
		}
		newCat := mimi

		newCat.friends = append(newCat.friends, "dddd")
		fmt.Println("now mimi is: ")
		fmt.Println(mimi)

		//manully deep copy
		xixi := Cat{
			10, "xixi", []string{"aaaa", "bbbb", "cccc"},
		}
		haha := xixi
		haha.friends = make([]string, len(xixi.friends))
		copy(haha.friends, xixi.friends)
		haha.friends = append(haha.friends, "dddd")

		fmt.Println(xixi)
		fmt.Println(haha)
	*/
}
