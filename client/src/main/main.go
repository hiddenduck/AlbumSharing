package main

import (
	"bufio"
	"fmt"
	"main/client"
	"os"
	"strings"
)

func main() {

	is_in_Album := true

	state := client.CreateClientState()

	for {

		reader := bufio.NewReader(os.Stdin)

		input, _ := reader.ReadString('\n')

		if input == "\n" {
			continue
		}

		input = strings.TrimSuffix(input, "\n")

		if input[0] == '/' {

			client.ExecuteCommand(strings.Split(input[1:], " "), state)

			continue
		}

		if !is_in_Album {
			fmt.Printf("Not associated with an album\n")

			continue
		}

		state.SessionState.CausalBroadcastInfo.CausalBroadcast([]byte(input))

		//fmt.Println(input)
		//state.Connector.Send_to_Peers("chat", []byte(input))
	}
}
