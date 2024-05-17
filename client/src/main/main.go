package main

import (
	"main/client"
	"os"
)

func main() {

	state := client.CreateClientState(os.Args[1])

	client.SpawnCentralServerThreads(&state)

	client.CommandListen(&state)
}
