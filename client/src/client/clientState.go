package client

import (
	centralservercomunication "main/CentralServerComunication"
	chat "main/connection_management"
	"main/crdt"
	"net"
)

type CommandMap map[string]interface{}

type SessionState struct {
	Replica             *crdt.Replica
	VoteMap             *map[string]bool
	Connector           *chat.ConnectorInfo
	CausalBroadcastInfo *chat.CausalBroadcastInfo
	MessageHandlers     map[string]interface{}
}

type ClientState struct {
	CommandMap              CommandMap
	IsInSession             bool
	IsLoggedIn              bool
	UserName                string
	SessionState            SessionState
	CentralServerConnection net.Conn
}

func CreateClientState() (clientState ClientState) {

	conn := centralservercomunication.ConnectToCentralServer()

	if err != nil {
		panic(err)
	}

	clientState = ClientState{
		CommandMap:              CreateCommandsMap(),
		IsInSession:             false,
		IsLoggedIn:              false,
		UserName:                "",
		SessionState:            SessionState{},
		CentralServerConnection: conn,
	}
	return
}

func CreateSessionState(clientId uint32) (sessionState SessionState) {
	//receive replica, votemap and clientId from central_server
	replica := crdt.CreateReplica(clientId)
	voteMap := make(map[string]bool)

	connector := chat.Make_ConnectorInfo()

	connector.SetIdentity(string(clientId))
	//
	connector.BindSocket("1111")
	//
	// connector.Add_Peer("PEER2", "Emanueldo Gonçalves Faria 2", "localhost", "2222")
	// connector.Add_Peer("PEER3", "Emanueldo Gonçalves Faria 3", "localhost", "3333")

	causalBI := chat.InitCausalBroadCast(clientId, &connector)

	messageHandlers := CreateMessageHandlers()

	messageHandlers["chat"] = ReceiveMsg
	messageHandlers["requestVV"] = SendCbcastVV

	go causalBI.CausalReceive(true)

	sessionState = SessionState{
		Replica:             &replica,
		VoteMap:             &voteMap,
		Connector:           &connector,
		CausalBroadcastInfo: &causalBI,
		MessageHandlers:     messageHandlers,
	}

	go HeartBeat(sessionState)
	go PeerListen(sessionState)

	return
}

func (sessionState SessionState) EndSession() {
	sessionState.Connector.Close()
}
