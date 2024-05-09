package main

import (
	"fmt"
)

func myprint(msg string) {
	fmt.Printf("%v\n", msg)
}

func CreateMessageHandlers() map[string]interface{} {
	return map[string]interface{}{
		"print": myprint,
	}
}

func PeerListen(messageHandlers map[string]interface{}, state ClientState) {
	for {
		msg, _ := state.Connector.RouterSocket.RecvMessage(0)

		function, ok := messageHandlers[msg[1]]

		if ok {
			function.(func(string))(msg[2])
		} else {
			fmt.Printf("\"%v\"; not a valid type of message!\n", msg[1])
		}
	}
}
