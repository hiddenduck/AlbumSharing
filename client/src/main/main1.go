package main

import (
	"bufio"
	"fmt"
	"main/client"
	chat "main/connection_management"
	"main/crdt"
	"os"
	"strings"
)

func createClientState(clientId uint32) client.ClientState {
	//receive replica, votemap and clientId from central_server
	replica := crdt.CreateReplica(clientId)
	voteMap := make(map[string]bool)

	connector := chat.Make_ConnectorInfo()

	connector.SetIdentity("PEER1")

	connector.BindSocket("1111")

	connector.Add_Peer("PEER2", "Emanueldo Gonçalves Faria 2", "localhost", "2222")
	connector.Add_Peer("PEER3", "Emanueldo Gonçalves Faria 3", "localhost", "3333")

	connector.Connect_to_Peers()

	return client.ClientState{Replica: &replica, VoteMap: &voteMap, Connector: &connector}
}

func main() {

	is_in_Album := true

	state := createClientState(0)

	state.Replica.AddFile("lmao", "hash0")

	go client.HeartBeat(state)

	go client.PeerListen(client.CreateMessageHandlers(), state)

	commandMap := client.CreateCommandsMap()

	for {

		reader := bufio.NewReader(os.Stdin)

		input, _ := reader.ReadString('\n')

		if input == "\n" {
			continue
		}

		input = strings.TrimSuffix(input, "\n")

		if input[0] == '/' {

			client.ExecuteCommand(strings.Split(input[1:], " "), commandMap, state)

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
