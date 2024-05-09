package main

import (
	"fmt"
	pb "main/CentralServerComunication/CentralServerProtoBuf"
	"main/crdt"
	"time"

	proto "google.golang.org/protobuf/proto"
)

func PeerListen(messageHandlers map[string]interface{}, state ClientState) {
	for {
		msg, _ := state.Connector.RouterSocket.RecvMessage(0)

		function, ok := messageHandlers[msg[1]]

		if ok {
			function.(func(string, ClientState))(msg[2], state)
		} else {
			fmt.Printf("\"%v\"; not a valid type of message!\n", msg[1])
		}
	}
}

func CreateMessageHandlers() map[string]interface{} {
	return map[string]interface{}{
		"chat":      myprint,
		"heartbeat": joinCrdt,
	}
}

func myprint(msg []byte, state ClientState) {
	fmt.Printf("%s\n", string(msg))
}

func parseProtoReplica(msg *pb.Crdt) crdt.Replica {
	crdtFiles := make(map[string]crdt.FileInfo)
	for filename := range msg.Files {
		fileInfo := msg.Files[filename]

		crdtVotes := make(map[uint32]crdt.VoteInfo)
		for id := range fileInfo.Votes {
			crdtVotes[id] = crdt.VoteInfo{Sum: fileInfo.Votes[id].Sum, Count: fileInfo.Votes[id].Count}
		}

		crdtDotSet := make(map[crdt.DotPair]bool)
		for dotPair := range fileInfo.DotSet {
			crdtDotSet[crdt.DotPair{dotPair.Id, dotPair.Version}] = true
		}

		crdtFiles[filename] = crdt.FileInfo{
			Votes:    crdtVotes,
			DotSet:   crdtDotSet,
			FileHash: fileInfo.FileHash,
		}
	}

	crdtGroupUsers := make(map[string]*pb.GroupInfo)
	for username := range state.Replica.GroupUsers {
		groupInfo := state.Replica.GroupUsers[username]

		crdtDotSet := make([]*pb.DotPair, 1)
		for dotPair := range groupInfo.DotSet {
			crdtDotSet = append(crdtDotSet, &pb.DotPair{Id: dotPair.Id, Version: dotPair.Version})
		}

		crdtGroupUsers[username] = &pb.GroupInfo{
			DotSet: crdtDotSet,
		}
	}
	return crdt.Replica{msg.VersionVector, msg.Id}
}

func joinCrdt(msg []byte, state ClientState) {
	protoMsg := pb.Crdt{}
	proto.Unmarshal(msg, &protoMsg)

	peerReplica = parseProtoReplica(&protoMsg)

	state.Replica.Converge(peerReplica)

}

func createCrdtMessage(state ClientState) *pb.Crdt {

	state.Mutex.Lock()

	defer state.Mutex.Unlock()

	crdtFiles := make(map[string]*pb.FileInfo)
	for filename := range state.Replica.Files {
		fileInfo := state.Replica.Files[filename]

		crdtVotes := make(map[uint32]*pb.VoteInfo)
		for id := range fileInfo.Votes {
			crdtVotes[id] = &pb.VoteInfo{Sum: fileInfo.Votes[id].Sum, Count: fileInfo.Votes[id].Count}
		}

		crdtDotSet := make([]*pb.DotPair, 1)
		for dotPair := range fileInfo.DotSet {
			crdtDotSet = append(crdtDotSet, &pb.DotPair{Id: dotPair.Id, Version: dotPair.Version})
		}

		crdtFiles[filename] = &pb.FileInfo{
			Votes:    crdtVotes,
			DotSet:   crdtDotSet,
			FileHash: fileInfo.FileHash,
		}
	}

	crdtGroupUsers := make(map[string]*pb.GroupInfo)
	for username := range state.Replica.GroupUsers {
		groupInfo := state.Replica.GroupUsers[username]

		crdtDotSet := make([]*pb.DotPair, 1)
		for dotPair := range groupInfo.DotSet {
			crdtDotSet = append(crdtDotSet, &pb.DotPair{Id: dotPair.Id, Version: dotPair.Version})
		}

		crdtGroupUsers[username] = &pb.GroupInfo{
			DotSet: crdtDotSet,
		}
	}

	p := &pb.Crdt{
		Files:         crdtFiles,
		GroupUsers:    crdtGroupUsers,
		VersionVector: state.Replica.VersionVector,
		Id:            state.Replica.CurrentID,
	}

	return p
}

func HeartBeat(state ClientState) {
	for {

		time.Sleep(time.Millisecond * 1000)

		out, err := proto.Marshal(createCrdtMessage(state))

		fmt.Println(out)

		if err != nil {
			panic("error")
		}

		state.Connector.Send_to_Peers("heartbeat", out)
	}
}
