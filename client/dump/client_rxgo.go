package main

import (
	"fmt"
	pb "main/protobuf"
	"net"
	"time"

	proto "google.golang.org/protobuf/proto"
)

const (
	SERVER_HOST = "localhost"
	SERVER_PORT = "1234"
	SERVER_TYPE = "tcp"
)

func main() {

    for {
        time.Sleep(time.Millisecond*1000)
        p := pb.FileMessage{
            Command: pb.CommandType_I_HAVE,
            Key:     []byte("lmao"),
            Data: &pb.FileMessage_Data{
                Bytes: []byte("lmao"),
                End:   false,
            },
        }

        out, err := proto.Marshal(&p)

        fmt.Println(out)

        if err != nil {
            panic("error")
        }

        connection, err := net.Dial(SERVER_TYPE, SERVER_HOST+":"+SERVER_PORT)

        if err != nil {
            panic("error")
        }

        len, err := connection.Write(out)

        fmt.Println(len)
    }


}

