package client

import (
	cs "main/CentralServerComunication"
	pb "main/CentralServerComunication/CentralServerProtobuf"
)

const (
	CHANNEL_BUFFER_SIZE = 1024
)

func HandleNewPeer(ch chan *pb.Message, state ClientState) {
	for msg := range ch {
		if state.IsInSession {
			m7 := msg.GetM7()
			state.SessionState.CausalBroadcastInfo.ConnectorInfo.Add_Peer(m7.Ip, m7.Name, m7.Ip, m7.Port)
		}
	}
}

func CreateCentralServerMessageHandlers() (handlers cs.Handlers) {
	handlers = make(cs.Handlers)

	typeList := []pb.Type{
		pb.Type_register,
		pb.Type_login,
		pb.Type_loginReply,
		pb.Type_create,
		pb.Type_get,
		pb.Type_send,
		pb.Type_quit,
		pb.Type_reply,
		pb.Type_new_peer,
		pb.Type_peer_left,
		pb.Type_newServer,
	}

	for _, pbType := range typeList {

		handlers[pbType] = make(chan *pb.Message, CHANNEL_BUFFER_SIZE)

	}

	return

}
