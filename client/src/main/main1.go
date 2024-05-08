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

    is_in_Album := true

    causalBroadcastInfo := chat.InitCausalBroadCast(1)

    go causalBroadcastInfo.Start_versionVector_server("1111")

    causalBroadcastInfo.ConnectorInfo.Add_Peer(2, "2", "localhost", "2222")

    causalBroadcastInfo.ConnectorInfo.Connect_to_Peers()

    go causalBroadcastInfo.CausalReceive(true)

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
        causalBroadcastInfo.CausalBroadcast([]byte(input))
	}
}
