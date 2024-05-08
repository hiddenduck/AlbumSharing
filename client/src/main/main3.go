package main

import (
	"bufio"
	"fmt"
	chat "main/connection_management"
	"os"
	"strings"
)

func main() {

	is_in_Album := true

	connector := chat.Make_ConnectorInfo()

	connector.SetIdentity("PEER3")

	connector.BindSocket("3333")

	connector.Add_Peer("PEER1", "Emanueldo Gonçalves Faria 1", "localhost", "1111")
	connector.Add_Peer("PEER2", "Emanueldo Gonçalves Faria 2", "localhost", "2222")

	connector.Connect_to_Peers()

	go connector.Listen_to_Peers()

	var commandMap map[string]interface{} = CreateCommandsMap()

	state := CreateClientState(2)

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
				function.(func([]string, ClientState))(list[1:], state)
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
