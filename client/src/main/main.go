package main

import (
	cs "main/CentralServerComunication"
	"main/client"
	"os"
)

func main() {

	state := client.CreateClientState(os.Args[0])

	go cs.CentralServerListener(state.CentralServerConnection, state.CentralServerMessageHandlers)

	client.CommandListen(&state)
}
