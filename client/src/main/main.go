package main

import (
	"bufio"
	"fmt"
	chat "main/connection_management"
	"os"
	"strings"
)

func myprint(l []string) {
	for range l {
		fmt.Println("foo")
	}
}

func main() {

	fmt.Println(os.Args[1])

    is_in_Album := true

    connector := chat.Make_ConnectorInfo()

    connector.Start_Publish(os.Args[1])

    // connector.Add_Peer("Emanueldo Gonçalves Faria", "localhost", os.Args[2])

    connector.Connect_to_Peers("")

    go connector.Listen_to_Peers()

	var commandMap map[string]interface{} = map[string]interface{}{
		"print": myprint,
	}

	for {

		reader := bufio.NewReader(os.Stdin)

		input, _ := reader.ReadString('\n')

		if input == "\n" {
			continue
		}

		input = strings.TrimSuffix(input, "\n")

		if input[0] == '/' {

			list := strings.Split(input[1:], " ")

			function, ok := commandMap[list[0]]

			if ok {
				function.(func([]string))(list[1:])
			} else {
				fmt.Printf("\"%v\"; not a valid command!\n", list[0])
			}

			continue
		}

		if !is_in_Album {
			fmt.Printf("Not associated with an album\n")

			continue
		}

        fmt.Println(input)
        connector.Send_to_Peers([]byte(input))
	}
}
