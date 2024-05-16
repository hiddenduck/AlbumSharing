package client

import (
	"fmt"
	centralservercomunication "main/CentralServerComunication"
	pb "main/CentralServerComunication/CentralServerProtobuf"
	dataservers "main/DataServers"
	"strconv"

	"google.golang.org/protobuf/proto"
)

func ExecuteCommand(list []string, state ClientState) {

	commandMap := state.CommandMap

	function, ok := commandMap[list[0]]

	if ok {
		//TODO não necessariamente fazer o lock aqui, é só para não me esquecer de fazer em todos os comandos
		//Não mata ninguém deixar aqui, só deixa comandos tipo salas mais lentos
		function.(func([]string, ClientState))(list[1:], state)
	} else {
		fmt.Printf("\"%v\"; not a valid command!\n", list[0])
	}
}

func addUser(msg []string, state ClientState) {
	state.SessionState.Replica.AddUser(msg[0])
}

func removeUser(msg []string, state ClientState) {
	state.SessionState.Replica.RemoveUser(msg[0])
}

func addFile(msg []string, state ClientState) {
	fileHash := dataservers.HashFile(msg[0])
	//TODO enviar para o dataserver
	state.SessionState.Replica.AddFile(msg[0], fileHash)
}

func removeFile(msg []string, state ClientState) {
	state.SessionState.Replica.RemoveFile(msg[0])
}

func rateFile(msg []string, client_state ClientState) {
	state := client_state.SessionState
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
	state.SessionState.Replica.ListUsers()
}

func listFiles(msg []string, state ClientState) {
	state.SessionState.Replica.ListFiles()
}

func listReplica(msg []string, state ClientState) {
	state.SessionState.Replica.ListReplica()
}

func downloadFile(msg []string, state ClientState) {
	//TODO download do ficheiro
	fmt.Printf("Undefined command.")
}

func printVV(msg []string, state ClientState) {
	state.SessionState.CausalBroadcastInfo.PrintVV()
}

func register(msg []string, state ClientState) {

	username := msg[0]
	password := msg[1]

	registerMessage := &pb.RegisterLoginFormat{
		UserName: username,
		Password: password,
	}

	message := pb.Message{
		Type: pb.Type_register,
		Msg: &pb.Message_M1{
			M1: registerMessage,
		},
	}

	data, err := proto.Marshal(&message)

	if err != nil {
		panic(err)
	}

	state.CentralServerConnection.Write(data)

	reply := &pb.Message{}

	buff := make([]byte, 1024)

	state.CentralServerConnection.Read(buff)

	proto.Unmarshal(buff, reply)

	status := reply.GetM5().Status

	if status == "register_ok" {
		fmt.Println("Register Success")
	} else {
		fmt.Println("Register failed, because NO")
	}
}

func login(msg []string, state ClientState) {

	username := msg[0]
	password := msg[1]

	loginMessage := &pb.RegisterLoginFormat{
		UserName: username,
		Password: password,
	}

	message := pb.Message{
		Type: pb.Type_login,
		Msg: &pb.Message_M1{
			M1: loginMessage,
		},
	}

	data, err := proto.Marshal(&message)

	if err != nil {
		panic(err)
	}

	state.CentralServerConnection.Write(data)

	reply := &pb.Message{}

	buff := make([]byte, 1024)

	state.CentralServerConnection.Read(buff)

	proto.Unmarshal(buff, reply)

	status := reply.GetM5().Status

	if status == "login_ok" {
		state.IsLoggedIn = true
		fmt.Println("Loggin Success")
	} else {
		fmt.Println("Login failed, invalid credentials")
	}
}

func getAlbum(msg []string, state ClientState){

    con := state.CentralServerConnection

    message := centralservercomunication.SessionStart(msg[0], con)

    state.SessionState = CreateSessionState(message.Id)

    *state.SessionState.Replica = ParseProtoReplica(message.Crdt)//???????
    state.SessionState.VoteMap = &message.VoteTable

    connector := state.SessionState.CausalBroadcastInfo.ConnectorInfo

    var isAlone bool

    if len(message.SessionPeers) == 0 {
        isAlone = true
    }else{
        isAlone = false
    }

    connector.SetIdentity(string(message.Id))

	connector.BindSocket(msg[1])

    for name, peerInfo := range message.SessionPeers {

        connector.Add_Peer(string(peerInfo.Id), name, peerInfo.Ip, peerInfo.Port)

    }

	connector.Connect_to_Peers()

	messageHandlers := CreateMessageHandlers()

	messageHandlers["chat"] = ReceiveMsg
	messageHandlers["requestVV"] = SendCbcastVV

	go state.SessionState.CausalBroadcastInfo.CausalReceive(isAlone)

	go HeartBeat(state.SessionState)

    go PeerListen(state.SessionState)

}

func CreateCommandsMap() CommandMap {
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
		"printVV":      printVV,
		"login":        login,
		"register":     register,
		"getAlbum":     getAlbum,
	}
}
