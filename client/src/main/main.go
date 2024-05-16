package main

import "main/client"

func main() {

	is_in_Album := true

	state := client.CreateSessionState()

	state.Replica.AddFile("lmao", "hash0")

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
