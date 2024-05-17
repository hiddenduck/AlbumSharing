package client

import (
	cs "main/CentralServerComunication"
	pb "main/CentralServerComunication/CentralServerProtobuf"
)

const (
	CHANNEL_BUFFER_SIZE = 1024
)

func SpawnCentralServerThreads(state *ClientState) {

	handlerMap := map[pb.Type]interface{}{
		pb.Type_new_peer:   HandleNewPeer,
		pb.Type_peer_left:  HandlePeerLeft,
		pb.Type_new_server: HandleNewServer,
	}

	for pbType := range handlerMap {
		go handlerMap[pbType].(func(chan *pb.Message, *ClientState))(state.CentralServerMessageHandlers[pbType], state)
	}

	go cs.CentralServerListener(state.CentralServerConnection, state.CentralServerMessageHandlers)
}

func HandleNewPeer(ch chan *pb.Message, state *ClientState) {
	for msg := range ch {
		if state.IsInSession.Load() {
			m7 := msg.GetM7()
			state.SessionState.CausalBroadcastInfo.ConnectorInfo.Add_Connect_Peer(m7.PeerInfo.Id, m7.Name, m7.PeerInfo.Ip, m7.PeerInfo.Port)
		}
	}
}

func HandlePeerLeft(ch chan *pb.Message, state *ClientState) {
	for msg := range ch {
		if state.IsInSession.Load() {
			m7 := msg.GetM7()
			state.SessionState.CausalBroadcastInfo.ConnectorInfo.Remove_Peer(m7.PeerInfo.Id, m7.Name, m7.PeerInfo.Ip, m7.PeerInfo.Port)
		}
	}
}

func HandleNewServer(ch chan *pb.Message, state *ClientState) {
	for msg := range ch {
		m8 := msg.GetM8()
		state.DataServers.AddServer(m8.Ip, m8.Port)
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
		pb.Type_new_server,
	}

	for _, pbType := range typeList {

		handlers[pbType] = make(chan *pb.Message, CHANNEL_BUFFER_SIZE)

	}

	return

}
