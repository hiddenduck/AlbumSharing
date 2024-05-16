package client

import (
	centralserver "main/CentralServerComunication"
	ds "main/DataServers"
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
	CommandMap                   CommandMap
	IsInSession                  bool
	IsLoggedIn                   bool
	UserName                     string
	SessionState                 SessionState
	CentralServerConnection      net.Conn
	DataServers                  ds.DataServers
	CentralServerMessageHandlers centralserver.Handlers
	CentralServerPort            string
	RouterPort                   string
}

func CreateClientState(routerPort string) (clientState ClientState) {

	conn := centralserver.ConnectToCentralServer()

	clientState = ClientState{
		CommandMap:                   CreateCommandsMap(),
		IsInSession:                  false,
		IsLoggedIn:                   false,
		UserName:                     "",
		SessionState:                 SessionState{},
		CentralServerConnection:      conn,
		DataServers:                  ds.InitDataServer(),
		CentralServerMessageHandlers: CreateCentralServerMessageHandlers(),
		RouterPort:                   routerPort,
	}
	return
}

func CreateSessionState(clientId uint32) (sessionState SessionState) {

	//receive replica, votemap and clientId from central_server
	replica := crdt.CreateReplica(clientId)
	voteMap := make(map[string]bool)

	connector := chat.Make_ConnectorInfo()

	causalBI := chat.InitCausalBroadCast(clientId, &connector)

	messageHandlers := CreateMessageHandlers()

	sessionState = SessionState{
		Replica:             &replica,
		VoteMap:             &voteMap,
		Connector:           &connector,
		CausalBroadcastInfo: &causalBI,
		MessageHandlers:     messageHandlers,
	}
	return
}
