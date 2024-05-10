package client

import (
	chat "main/connection_management"
	"main/crdt"
)

type ClientState struct {
	Replica             *crdt.Replica
	VoteMap             *map[string]bool
	Connector           *chat.ConnectorInfo
	CausalBroadcastInfo *chat.CausalBroadcastInfo
	MessageHandlers     map[string]interface{}
}
