package centralservercomunication

import (
	"fmt"
	pb "main/CentralServerComunication/CentralServerProtoBuf"
	"net"

	proto "google.golang.org/protobuf/proto"
)

const (
	SERVER_HOST = "localhost"
	SERVER_PORT = "9988"
	SERVER_TYPE = "tcp"
)

func test() {

	data := &pb.Message{
		Type: pb.Type_register,
		Msg: &pb.Message_M1{
			M1: &pb.RegisterLoginFormat{
				UserName: "UserName",
				Password: "Password",
			},
		},
	}

	out, err := proto.Marshal(data)

	if err != nil {
		panic("lmao")
	}

	//establish connection
	connection, err := net.Dial(SERVER_TYPE, SERVER_HOST+":"+SERVER_PORT)
	if err != nil {
		panic(err)
	}

	///send some data
	_, err = connection.Write(out)

	buffer := make([]byte, 1024)

	connection.Read(buffer)

    data_out := &pb.Message{}

    proto.Unmarshal(buffer, data_out)

    fmt.Println(data_out.Type)

	defer connection.Close()

}
