package centralservercomunication

import (
	"fmt"
	pb "main/CentralServerComunication/CentralServerProtobuf"
	"net"

	proto "google.golang.org/protobuf/proto"
)

const (
	SERVER_HOST = "localhost"
	SERVER_PORT = "8888"
	SERVER_TYPE = "tcp"
	BUFFER_SIZE = 1024 * 100
)

type Handlers map[pb.Type]chan *pb.Message

func CentralServerListener(connection net.Conn, handlers Handlers) {

	for {

        buff := make([]byte, BUFFER_SIZE)

		_, err := connection.Read(buff)

		if err != nil {
			panic(err)
		}

		msg := &pb.Message{}

		proto.Unmarshal(buff, msg)


        fmt.Printf("msg.Type: %v\n", msg.Type)

		ch, ok := handlers[msg.Type]

        //non blocking chanel
		if ok {
            ch <- msg
            fmt.Printf("Sent to peep\n")
			// select {
   //          case ch <- msg:
   //              fmt.Printf("sent message %+v\n", msg)
   //          default:
   //          }
        }

	}

}

func ConnectToCentralServer() (connection net.Conn) {

	fmt.Printf("Dialing Central Server on port: %v\n", SERVER_PORT)
	connection, err := net.Dial(SERVER_TYPE, SERVER_HOST+":"+SERVER_PORT)
	fmt.Printf("Connected to Central Server with connection: %v\n", connection)

	if err != nil {
		panic(err)
	}

	return
}

func SessionStart(albumName string, conn net.Conn) *pb.SessionStart {

	message := &pb.Message{
		Type: pb.Type_get,
		Msg: &pb.Message_M2{
			M2: &pb.Album{
				AlbumName: albumName,
			},
		},
	}

	data, err := proto.Marshal(message)

	if err != nil {
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
