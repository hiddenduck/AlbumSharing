package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func myprint(l []string) {
    for range l {
        fmt.Println("foo")
    }
}

func myprint2(l []string) {
    for range l {
        fmt.Println("bar")
    }
}

var commandMap map[string]interface{} = map[string]interface{}{
    "print": myprint,
    "bar": myprint2,
}

func main() {

	for {

		reader := bufio.NewReader(os.Stdin)

		input, _ := reader.ReadString('\n')

        input = strings.TrimSuffix(input, "\n")

		if input[0] == '/' {

			list := strings.Split(input[1:], " ")

            function, ok := commandMap[list[0]]

            if ok {
                function.(func([]string))(list[1:])
            }

		}

	}
}
