package main

import (
	"fmt"
	"strconv"
)

func addUser(msg []string, state ClientState) {
	state.Replica.AddUser(msg[0])
}

func removeUser(msg []string, state ClientState) {
	state.Replica.RemoveUser(msg[0])
}

func addFile(msg []string, state ClientState) {
	state.Replica.AddFile(msg[0])
}

func removeFile(msg []string, state ClientState) {
	state.Replica.RemoveFile(msg[0])
}

func addClassification(msg []string, state ClientState) {
	classification, err := strconv.ParseUint(msg[1], 10, 64)
	if err != nil {
		state.Replica.AddUserClassification(msg[0], classification, state.VoteMap)
	} else {
		fmt.Printf("\"%v\"; not a valid uint64!\n", msg[1])
	}
}

func test(msg []string, state ClientState) {
	for i, x := range msg {
		fmt.Printf("frame %v: %v\n", i, x)
	}
}

func CreateCommandsMap() map[string]interface{} {
	return map[string]interface{}{
		"test":              test,
		"addUser":           addUser,
		"removeUser":        removeUser,
		"addFile":           addFile,
		"removeFile":        removeFile,
		"addClassification": addClassification,
	}
}
