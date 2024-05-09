package main

import (
	"bufio"
	"fmt"
	chat "main/connection_management"
	"main/crdt"
	"os"
	"strings"
)

func createClientState(clientId uint32) ClientState {
	//receive replica, votemap and clientId from central_server
	replica := crdt.CreateReplica(clientId)
	voteMap := make(map[string]bool)

func main() {

	connector.Add_Peer("PEER2", "Emanueldo Gonçalves Faria 2", "localhost", "2222")
	connector.Add_Peer("PEER3", "Emanueldo Gonçalves Faria 3", "localhost", "3333")

    causalBroadcastInfo := chat.InitCausalBroadCast(1)

    go causalBroadcastInfo.Start_versionVector_server("1110")

    causalBroadcastInfo.ConnectorInfo.BindSocket("1111")

    causalBroadcastInfo.ConnectorInfo.SetIdentity("Peer1")

    causalBroadcastInfo.ConnectorInfo.Add_Peer("Peer2", "2", "localhost", "2222", "2220")
    causalBroadcastInfo.ConnectorInfo.Add_Peer("Peer3", "3", "localhost", "3333", "3330")

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
