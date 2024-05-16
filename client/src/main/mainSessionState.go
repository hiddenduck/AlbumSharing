package main

import "main/client"

func main() {
	state := client.CreateClientState()
	client.CommandListen(state)
}
