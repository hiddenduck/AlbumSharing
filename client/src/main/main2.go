package main

import (
	"bufio"
	"fmt"
	chat "main/connection_management"
<<<<<<< HEAD
	"os"
	"strings"
)

func createClientState(clientId uint32) ClientState {
	//receive replica, votemap and clientId from central_server
	replica := crdt.CreateReplica(clientId)
	voteMap := make(map[string]bool)

func main() {

	is_in_Album := true

	connector.Connect_to_Peers()

	go causalBroadcastInfo.Start_versionVector_server("2220")

	causalBroadcastInfo.ConnectorInfo.BindSocket("2222")

	causalBroadcastInfo.ConnectorInfo.SetIdentity("Peer2")

	causalBroadcastInfo.ConnectorInfo.Add_Peer("Peer1", "1", "localhost", "1111", "1110")
	causalBroadcastInfo.ConnectorInfo.Add_Peer("Peer3", "3", "localhost", "3333", "3330")

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

		//fmt.Println(input)
		state.Connector.Send_to_Peers("chat", []byte(input))
	}
}
