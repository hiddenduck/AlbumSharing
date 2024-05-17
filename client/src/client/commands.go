package client

import (
	"bufio"
	"fmt"
	pb "main/CentralServerComunication/CentralServerProtobuf"
	dataservers "main/DataServers"
	"net"
	"os"
	"strconv"
	"strings"

	"google.golang.org/protobuf/proto"
)

func CommandListen(state *ClientState) {
	for {

		reader := bufio.NewReader(os.Stdin)

		input, _ := reader.ReadString('\n')

		if input == "\n" {
			continue
		}

		input = strings.TrimSuffix(input, "\n")

		if input[0] == '/' {

			ExecuteCommand(strings.Split(input[1:], " "), state)

			continue
		}

		if !state.IsInSession.Load() {
			fmt.Printf("Not associated with an album\n")

			continue
		}

		state.SessionState.CausalBroadcastInfo.CausalBroadcast([]byte(input))

		//state.Connector.Send_to_Peers("chat", []byte(input))
	}
	fmt.Println("Nunca devia ter chegado aqui")
}

func ExecuteCommand(list []string, state *ClientState) {

	commandMap := state.CommandMap

	function, ok := commandMap[list[0]]

	if ok {
		//TODO não necessariamente fazer o lock aqui, é só para não me esquecer de fazer em todos os comandos
		//Não mata ninguém deixar aqui, só deixa comandos tipo salas mais lentos
		function.(func([]string, *ClientState))(list[1:], state)
	} else {
		fmt.Printf("\"%v\"; not a valid command!\n", list[0])
	}
}

func addUser(msg []string, state *ClientState) {
	if !state.IsInSession.Load() {
		fmt.Printf("Not connected to a session, connect first before storing.\n")
		return
	}
	if len(msg) != 1 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}
	state.SessionState.Replica.AddUser(msg[0])
}

func removeUser(msg []string, state *ClientState) {
	if !state.IsInSession.Load() {
		fmt.Printf("Not connected to a session, connect first before storing.\n")
		return
	}
	if len(msg) != 1 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}
	state.SessionState.Replica.RemoveUser(msg[0])
}

func addFile(msg []string, state *ClientState) {

	if !state.IsInSession.Load() {
		fmt.Printf("Not connected to a session, connect first before storing.\n")
		return
	}

	if len(msg) != 1 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}

	fileHash, err := dataservers.HashFile(msg[0])
	if err != nil {
		return
	}
	//dataservers.UploadFile(state.DataServers, msg[0])
	//TODO enviar para o dataserver
    go func() {
        dataservers.UploadFile(state.DataServers, msg[0])

        state.SessionState.Replica.AddFile(msg[0], fileHash)
    }()

}

func removeFile(msg []string, state *ClientState) {
	if !state.IsInSession.Load() {
		fmt.Printf("Not connected to a session, connect first before storing.\n")
		return
	}

	if len(msg) != 1 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}
	state.SessionState.Replica.RemoveFile(msg[0])
}

func rateFile(msg []string, client_state *ClientState) {
	if !client_state.IsInSession.Load() {
		fmt.Printf("Not connected to a session, connect first before storing.\n")
		return
	}

	if len(msg) != 2 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}

	state := client_state.SessionState
	classification, err := strconv.ParseUint(msg[1], 10, 64)
	if err == nil {
		if classification > 5 || classification < 0 {
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

func test(msg []string, state *ClientState) {
	for i, x := range msg {
		fmt.Printf("frame %v: %v\n", i, x)
	}
}

func listUsers(msg []string, state *ClientState) {
	if state.IsInSession.Load() {
		state.SessionState.Replica.ListUsers()
	}
}

func listFiles(msg []string, state *ClientState) {
	if state.IsInSession.Load() {
		state.SessionState.Replica.ListFiles()
	}
}

func listReplica(msg []string, state *ClientState) {
	if state.IsInSession.Load() {
		state.SessionState.Replica.ListReplica()
	}
}

func downloadFile(msg []string, state *ClientState) {
	//TODO download do ficheiro
	if state.IsInSession.Load() {
		dataservers.DownLoadFile(state.DataServers, msg[0], state.SessionState.Replica.GetFileHash(msg[0]))
	}
}

func printVV(msg []string, state *ClientState) {
	if state.IsInSession.Load() {
		state.SessionState.CausalBroadcastInfo.PrintVV()
	}
}

func register(msg []string, state *ClientState) {

	fmt.Printf("Started Register\n")

	if len(msg) != 2 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}

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
	fmt.Printf("Sent register to Central Server\n")

	// buff := make([]byte, 1024)

	// state.CentralServerConnection.Read(buff)

	reply := <-state.CentralServerMessageHandlers[pb.Type_reply]

	fmt.Printf("reply.Type: %v\n", reply.Type)

	fmt.Printf("Received reply from Central Server\n")

	// proto.Unmarshal(buff, reply)

	status := reply.GetM5().Status

	if status == "register_ok" {
		fmt.Println("Register Success")
	} else {
		fmt.Println("Register failed, because NO")
	}
}

func login(msg []string, state *ClientState) {

	fmt.Printf("Started Login\n")

	if len(msg) != 2 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}

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

	fmt.Printf("Sent login to Central Server\n")

	// reply := &pb.Message{}

	// buff := make([]byte, 1024)

	reply := <-state.CentralServerMessageHandlers[pb.Type_loginReply]

	fmt.Printf("Received login from Central Server\n")

	// proto.Unmarshal(buff, reply)

	status := reply.GetM6().Status

	if status == "login_ok" {
		for _, info := range reply.GetM6().DataServers {
			state.DataServers.AddServer(info.Ip, info.Port)
		}

		state.IsLoggedIn = true
		fmt.Println("Loggin Success")
	} else {
		fmt.Println("Login failed, invalid credentials")
	}

}

func createAlbum(msg []string, state *ClientState) {

	if len(msg) != 1 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}

	albumName := msg[0]

	createAlbumMessage := &pb.Album{
		AlbumName: albumName,
	}

	message := pb.Message{
		Type: pb.Type_create,
		Msg: &pb.Message_M2{
			M2: createAlbumMessage,
		},
	}

	data, err := proto.Marshal(&message)

	if err != nil {
		panic(err)
	}

	state.CentralServerConnection.Write(data)

	fmt.Printf("Sent create to Central Server\n")

	reply := <-state.CentralServerMessageHandlers[pb.Type_reply]

	fmt.Printf("Received create from Central Server\n")

	status := reply.GetM5().Status

	if status == "create_album_ok" {
		fmt.Printf("Created Album %v\n", albumName)
	} else {
		fmt.Println("Create failed, album name already exists!")
	}
}

func sessionStart(albumName string, conn net.Conn, state *ClientState) *pb.SessionStart {

	message := &pb.Message{
		Type: pb.Type_get,
		Msg: &pb.Message_M2{
			M2: &pb.Album{
				AlbumName: albumName,
				Port:      state.RouterPort,
			},
		},
	}

	data, err := proto.Marshal(message)

	if err != nil {
		panic(err)
	}

	conn.Write(data)

	fmt.Printf("Sent request to session to Central Server\n")

	reply := <-state.CentralServerMessageHandlers[pb.Type_send]

	fmt.Printf("Received session from Central Server\n")

	m3 := reply.GetM3()

	return m3
}

func getAlbum(msg []string, state *ClientState) {

	if state.IsInSession.Load() {
		fmt.Printf("Already connected to a session, leave first before reconnecting.\n")
		return
	}

	if len(msg) != 1 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}

	con := state.CentralServerConnection

	message := sessionStart(msg[0], con, state)

	if message.Status != "get_album_ok" {
		fmt.Println("You don't have permission to edit this album!")
		return
	}

	state.SessionState = CreateSessionState(message.Id)

	*state.SessionState.Replica = ParseProtoReplica(message.Crdt) //???????
	if message.VoteTable != nil {
		state.SessionState.VoteMap = &message.VoteTable
	} else {
		m := make(map[string]bool)
		state.SessionState.VoteMap = &m
	}

	connector := state.SessionState.CausalBroadcastInfo.ConnectorInfo

	var isAlone bool

	if len(message.SessionPeers) == 0 {
		isAlone = true
	} else {
		isAlone = false
	}

	connector.SetIdentity(fmt.Sprintf("PEER%v", message.Id))

	connector.BindSocket(state.RouterPort)

	for name, peerInfo := range message.SessionPeers {

		connector.Add_Peer(fmt.Sprintf("PEER%v", peerInfo.Id), name, peerInfo.Ip, peerInfo.Port)

	}

	connector.Connect_to_Peers()

	state.SessionState.CausalBroadcastInfo.CausalReceive(isAlone)

	if isAlone {
		state.SessionState.MessageHandlers["chat"] = ReceiveMsg
		state.SessionState.MessageHandlers["requestVV"] = SendCbcastVV
	}

	go HeartBeat(state.SessionState)

	go PeerListen(state.SessionState)

	state.IsInSession.Store(true)
}

func putAlbum(msg []string, state *ClientState) {

	if !state.IsInSession.Load() {
		fmt.Printf("Not connected to a session, connect first before storing.\n")
		return
	}

	if len(msg) != 0 {
		fmt.Printf("Argument list is not the correct and it is therefore the wrong\n")
		return
	}

	message := &pb.Message{
		Type: pb.Type_quit,
		Msg: &pb.Message_M4{
			M4: &pb.QuitMessage{
				Crdt:      createCrdtMessage(state.SessionState),
				VoteTable: *state.SessionState.VoteMap,
			},
		},
	}

	data, err := proto.Marshal(message)

	if err != nil {
		panic(err)
	}

	state.CentralServerConnection.Write(data)

	fmt.Printf("Sent request to session to Central Server\n")

	reply := <-state.CentralServerMessageHandlers[pb.Type_reply]

	fmt.Printf("Received session from Central Server\n")

	status := reply.GetM5().Status

	if status == "put_album_ok" { // discard replica

		state.IsInSession.Store(false)
		state.SessionState.Connector.Close()
		fmt.Println("Session Has Ended")

	} else {
		fmt.Println("Session failed to end")
	}

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
		"createAlbum":  createAlbum,
		"getAlbum":     getAlbum,
		"putAlbum":     putAlbum,
	}
}
