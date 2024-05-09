package main

import (
	"fmt"
	"strconv"
)

func ExecuteCommand(list []string, commandMap map[string]interface{}, state ClientState) {
	function, ok := commandMap[list[0]]

	if ok {
		//TODO não necessariamente fazer o lock aqui, é só para não me esquecer de fazer em todos os comandos
		//Não mata ninguém deixar aqui, só deixa comandos tipo salas mais lentos
		state.Mutex.Lock()
		defer state.Mutex.Unlock()
		function.(func([]string, ClientState))(list[1:], state)
	} else {
		fmt.Printf("\"%v\"; not a valid command!\n", list[0])
	}
}

func addUser(msg []string, state ClientState) {
	state.Replica.AddUser(msg[0])
}

func removeUser(msg []string, state ClientState) {
	state.Replica.RemoveUser(msg[0])
}

func addFile(msg []string, state ClientState) {
	//TODO calcular o hash
	state.Replica.AddFile(msg[0], "hash_test_1")
}

func removeFile(msg []string, state ClientState) {
	state.Replica.RemoveFile(msg[0])
}

func rateFile(msg []string, state ClientState) {
	classification, err := strconv.ParseUint(msg[1], 10, 64)
	if err == nil {
		if classification > 5 || classification < 1 {
			fmt.Printf("%v; not a valid clasification, enter a value between 1 and 5!\n", msg[1])
		} else {
			fileExists, cantVote := state.Replica.AddUserClassification(msg[0], classification, state.VoteMap)
			if !fileExists {
				fmt.Printf("%v; File does not exist!\n", msg[0])
			} else if cantVote {
				fmt.Printf("%v; Already voted for file!\n", msg[0])
			}
		}
	} else {
		fmt.Printf("%v; not a valid uint64!\n", msg[1])
	}
}

func test(msg []string, state ClientState) {
	for i, x := range msg {
		fmt.Printf("frame %v: %v\n", i, x)
	}
}

func listUsers(msg []string, state ClientState) {
	state.Replica.ListUsers()
}

func listFiles(msg []string, state ClientState) {
	state.Replica.ListFiles()
}

func listReplica(msg []string, state ClientState) {
	state.Replica.ListReplica()
}

func downloadFile(msg []string, state ClientState) {
	//TODO download do ficheiro
	fmt.Printf("Undefined command.")
}

func CreateCommandsMap() map[string]interface{} {
	return map[string]interface{}{
		"downloadFile": downloadFile,
		"listUsers":    listUsers,
		"listFiles":    listFiles,
		"listReplica":  listReplica,
		"test":         test,
		"addUser":      addUser,
		"removeUser":   removeUser,
		"addFile":      addFile,
		"removeFile":   removeFile,
		"rateFile":     rateFile,
	}
}
