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

	replica := crdt.CreateReplica(clientId)
	voteMap := make(map[string]bool)

	connector := chat.Make_ConnectorInfo()

	connector.SetIdentity("PEER3")

	connector.BindSocket("3333")

	connector.Add_Peer("PEER1", "Emanueldo Gonçalves Faria 1", "localhost", "1111")
	connector.Add_Peer("PEER2", "Emanueldo Gonçalves Faria 2", "localhost", "2222")

	connector.Connect_to_Peers()

	causalBI := chat.InitCausalBroadCast(clientId, &connector)

	causalBI.CausalReceive(false)

	return client.ClientState{Replica: &replica, VoteMap: &voteMap, Connector: &connector, CausalBroadcastInfo: &causalBI, MessageHandlers: client.CreateMessageHandlers()}
}

func main() {

	is_in_Album := true

	state := createClientState(2)

	go client.HeartBeat(state)

	go client.PeerListen(state)

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

		state.CausalBroadcastInfo.CausalBroadcast([]byte(input))

		//fmt.Println(input)
		//state.Connector.Send_to_Peers("chat", []byte(input))
	}
}
