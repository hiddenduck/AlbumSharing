package client

import (
	cs "main/CentralServerComunication"
	pb "main/CentralServerComunication/CentralServerProtobuf"
)

const(
    CHANNEL_BUFFER_SIZE = 1024
)

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

    for _, pbType := range typeList{

        handlers[pbType] = make(chan *pb.Message, CHANNEL_BUFFER_SIZE)

    }

    return

}
