package main

import (
	"bufio"
	"fmt"
	chat "main/connection_management"
	"os"
	"strings"
)

func createClientState(clientId uint32) ClientState {
	connector := chat.Make_ConnectorInfo()

	connector.SetIdentity("PEER3")

	connector.BindSocket("3333")

	connector.Add_Peer("PEER1", "Emanueldo Gonçalves Faria 1", "localhost", "1111")
	connector.Add_Peer("PEER2", "Emanueldo Gonçalves Faria 2", "localhost", "2222")

	connector.Connect_to_Peers()

	go connector.Listen_to_Peers(CreateMessageHandlers())

}

func main() {

	is_in_Album := true

	state := createClientState(2)

	go HeartBeat(state)

	go PeerListen(CreateMessageHandlers(), state)

	commandMap := CreateCommandsMap()

	for {

		reader := bufio.NewReader(os.Stdin)

		input, _ := reader.ReadString('\n')

		if input == "\n" {
			continue
		}

		input = strings.TrimSuffix(input, "\n")

		if input[0] == '/' {

			ExecuteCommand(strings.Split(input[1:], " "), commandMap, state)

			continue
		}

		if !is_in_Album {
			fmt.Printf("Not associated with an album\n")

			continue
		}

		//fmt.Println(input)
		state.Connector.Send_to_Peers("chat", []byte(input))
	}
}
