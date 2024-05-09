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

	causalBroadcastInfo := chat.InitCausalBroadCast(3)

	go causalBroadcastInfo.Start_versionVector_server("3330")

	causalBroadcastInfo.ConnectorInfo.BindSocket("3333")

	causalBroadcastInfo.ConnectorInfo.SetIdentity("Peer3")

	causalBroadcastInfo.ConnectorInfo.Add_Peer("Peer2", "2", "localhost", "2222", "2220")
	causalBroadcastInfo.ConnectorInfo.Add_Peer("Peer1", "1", "localhost", "1111", "1110")

	causalBroadcastInfo.ConnectorInfo.Connect_to_Peers()

	go causalBroadcastInfo.CausalReceive(false)

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

		fmt.Println(input)
		causalBroadcastInfo.CausalBroadcast([]byte(input))
	}
}
