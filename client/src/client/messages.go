package client

import (
	"fmt"
	pb "main/CentralServerComunication/CentralServerProtobuf"
	dataservers "main/DataServers"
	"main/crdt"
	"sync"
	"time"

	proto "google.golang.org/protobuf/proto"
)

func PeerListen(state SessionState) {
	for {
		msg, _ := state.Connector.RouterSocket.RecvMessage(0)

		function, ok := state.MessageHandlers[msg[1]]

		if msg[1] != "heartbeat" {
			fmt.Printf("Received message of type %v from peer %v\n", msg[1], msg[0])
		}

		if ok {
			function.(func([]string, SessionState))(msg, state)
		} else {
			fmt.Printf("\"%v\"; not a valid type of message!\n", msg[1])
		}
	}
}

func CreateMessageHandlers() map[string]interface{} {
	return map[string]interface{}{
		"chat":      bufferMsg,
		"heartbeat": joinCrdt,
		"requestVV": bufferRequest,
		"replyVV":   receiveVV,
	}
}

func myprint(msg []string, state SessionState) {
	fmt.Printf("%s\n", msg[2])
}

func bufferMsg(msg []string, state SessionState) {
	state.CausalBroadcastInfo.Buffer_message(msg)
}

func ReceiveMsg(msg []string, state SessionState) {
	state.CausalBroadcastInfo.Fwd_message(msg)
}

func joinCrdt(msg []string, state SessionState) {
	protoMsg := pb.Crdt{}
	proto.Unmarshal([]byte(msg[2]), &protoMsg)

	peerReplica := ParseProtoReplica(&protoMsg)

	//fmt.Printf("Recebido Heartbeat do CRDT do nodo %v\n", protoMsg.Id)

	state.Replica.Converge(peerReplica, state.VoteMap)
}

func bufferRequest(msg []string, state SessionState) {
	state.CausalBroadcastInfo.AddVVRequest(msg[0])
}

func SendCbcastVV(msg []string, state SessionState) {
	state.CausalBroadcastInfo.SendVersionVector(msg[0])
}

func receiveVV(msg []string, state SessionState) {
	state.CausalBroadcastInfo.ReceiveVV([]byte(msg[2]))
	state.MessageHandlers["chat"] = ReceiveMsg
	state.MessageHandlers["requestVV"] = SendCbcastVV
}

func HeartBeat(state SessionState) {
	for {

		time.Sleep(time.Millisecond * 1000)

		out, err := proto.Marshal(createCrdtMessage(state))

		if err != nil {
			panic("error")
		}

		state.Connector.Send_to_Peers("heartbeat", out)

		//fmt.Println("Enviado Heartbeat do CRDT")
	}
}

func createCrdtMessage(state SessionState) *pb.Crdt {

	state.Replica.Mutex.Lock()

	defer state.Replica.Mutex.Unlock()

	crdtFiles := make(map[string]*pb.FileInfo)
	for filename := range state.Replica.Files {
		fileInfo := state.Replica.Files[filename]

		crdtVotes := make(map[uint32]*pb.VoteInfo)
		for id := range fileInfo.Votes {
			crdtVotes[id] = &pb.VoteInfo{Sum: fileInfo.Votes[id].Sum, Count: fileInfo.Votes[id].Count}
		}

		crdtDotSet := make([]*pb.DotPair, 0)
		for dotPair := range fileInfo.DotSet {
			crdtDotSet = append(crdtDotSet, &pb.DotPair{Id: dotPair.Id, Version: dotPair.Version})
		}

		crdtFiles[filename] = &pb.FileInfo{
			Votes:    crdtVotes,
			DotSet:   crdtDotSet,
			FileHash: string(fileInfo.FileHash[:]),
		}
	}

	crdtGroupUsers := make(map[string]*pb.GroupInfo)
	for username := range state.Replica.GroupUsers {
		groupInfo := state.Replica.GroupUsers[username]

		crdtDotSet := make([]*pb.DotPair, 0)
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

func ParseProtoReplica(msg *pb.Crdt) crdt.Replica {

	fmt.Printf("file: %v\n", msg.Files)

	crdtFiles := make(map[string]crdt.FileInfo)
	for filename := range msg.Files {

		fileInfo := msg.Files[filename]

		crdtVotes := make(map[uint32]crdt.VoteInfo)
		for id := range fileInfo.Votes {
			crdtVotes[id] = crdt.VoteInfo{Sum: fileInfo.Votes[id].Sum, Count: fileInfo.Votes[id].Count}
		}

		crdtDotSet := make(map[crdt.DotPair]bool)
		for _, dotPair := range fileInfo.DotSet {
			crdtDotSet[crdt.DotPair{Id: dotPair.Id, Version: dotPair.Version}] = true
		}

		crdtFiles[filename] = crdt.FileInfo{
			Votes:    crdtVotes,
			DotSet:   crdtDotSet,
			FileHash: dataservers.Hash([]byte(fileInfo.FileHash)),
		}
	}

	crdtGroupUsers := make(map[string]crdt.GroupInfo)
	for username := range msg.GroupUsers {
		groupInfo := msg.GroupUsers[username]

		crdtDotSet := make(map[crdt.DotPair]bool)
		for _, dotPair := range groupInfo.DotSet {
			crdtDotSet[crdt.DotPair{Id: dotPair.Id, Version: dotPair.Version}] = true
		}

		crdtGroupUsers[username] = crdt.GroupInfo{
			DotSet: crdtDotSet,
		}
	}

	return crdt.Replica{Files: crdtFiles, GroupUsers: crdtGroupUsers, VersionVector: msg.VersionVector, CurrentID: msg.Id, Mutex: &sync.Mutex{}}
}
