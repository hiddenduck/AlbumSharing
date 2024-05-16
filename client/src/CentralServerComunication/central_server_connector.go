package centralservercomunication

import (
	pb "main/CentralServerComunication/CentralServerProtobuf"
	"net"

	proto "google.golang.org/protobuf/proto"
)

const (
	SERVER_HOST = "localhost"
	SERVER_PORT = "8888"
	SERVER_TYPE = "tcp"
)

func ConnectToCentralServer() (connection net.Conn) {

	connection, err := net.Dial(SERVER_TYPE, SERVER_HOST+":"+SERVER_PORT)

    if err != nil{
        panic(err)
    }

    return 
}

func SessionStart(albumName string, conn net.Conn) (*pb.SessionStart) {

    message := &pb.Message{
        Type: pb.Type_get,
        Msg: &pb.Message_M2{
            M2: &pb.Album{
                AlbumName: albumName,
            },
        },
    }

    data, err := proto.Marshal(message)

    if err != nil{
        panic(err)
    }

    conn.Write(data)


    buf := make([]byte, 1024)

    reply := &pb.Message{}
    conn.Read(buf)

    proto.Unmarshal(buf, reply)

    m3 := reply.GetM3()

    return m3

}

