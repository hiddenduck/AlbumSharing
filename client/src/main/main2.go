package main

import (
	"bufio"
	"fmt"
	chat "main/connection_management"
	"main/crdt"
	"os"
	"strings"
	"sync"
)

func createClientState(clientId uint32) ClientState {
	//receive replica, votemap and clientId from central_server
	replica := crdt.CreateReplica(clientId)
	voteMap := make(map[string]bool)

	connector := chat.Make_ConnectorInfo()

	connector.SetIdentity("PEER2")

	connector.BindSocket("2222")

	connector.Add_Peer("PEER1", "Emanueldo Gonçalves Faria 1", "localhost", "1111")
	connector.Add_Peer("PEER3", "Emanueldo Gonçalves Faria 3", "localhost", "3333")

	connector.Connect_to_Peers()

	return ClientState{Replica: &replica, VoteMap: &voteMap, Connector: &connector, Mutex: &sync.Mutex{}}
}

func main() {

	is_in_Album := true

	state := createClientState(1)

	state.Replica.AddFile("lmao", "hash1")

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
