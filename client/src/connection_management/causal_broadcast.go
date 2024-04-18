package connectionmanagement

import (
	"fmt"
	proto "google.golang.org/protobuf/proto"
	pb "main/connection_management/cbCastProtobuf"
)

// func main() {
//
//     array := []uint64{1,2,3,4,5,6,7,8,9,10,11,12,13,14}
//
//     p := protobuf.CbCastMessage{
//         Type: protobuf.MessageType_FORWARD_MSG,
//         VersionVector: array,
//         Data: []byte("lmao"),
//     }
//
//     out, _ := proto.Marshal(&p)
//
//     fmt.Println(out)
//
//     p_ := protobuf.CbCastMessage{}
//
//     proto.Unmarshal(out, &p_)
//
//     fmt.Println(p_.VersionVector)
//
// }

func CausalReceive(connector ConnectorInfo) {

	ch := make(chan []byte)

	go fwd_message(connector, ch)

    for msg := range ch{
        fmt.Println(string(msg))
    }

}

func test_msg(src uint32, versionVector []uint64, data []byte) bool {
	return false
}

func unpack_msg(msg *pb.CbCastMessage) (src uint32, vv []uint64, data []byte) {
	//existe getters do protobuf, nao sei qual a melhor alternativa
	src = msg.Src
	vv = msg.VersionVector
	data = msg.Data
	return
}

func fwd_message(connector ConnectorInfo, ch chan []byte) {

	//this is a set
	var buffer = make(map[*pb.CbCastMessage]struct{}, 0)

	for {

		bytes, _ := connector.SubSocket.RecvBytes(0)

		msg := pb.CbCastMessage{}

		proto.Unmarshal(bytes, &msg)

		src, versionVector, data := unpack_msg(&msg)

		if test_msg(src, versionVector, data) {

			ch <- data

			for buffered_msg := range buffer {

				src, versionVector, data = unpack_msg(buffered_msg)

				if test_msg(src, versionVector, data) {
					ch <- data
					delete(buffer, &msg)
				}
			}

		} else {
			buffer[&msg] = struct{}{}
		}
	}

}

func CausalBroadcast(self uint32, connector ConnectorInfo, msg []byte, versionVector []uint64) {

	data := pb.CbCastMessage{
		Src:           self,
		VersionVector: versionVector,
		Data:          msg,
	}

	bytes, _ := proto.Marshal(&data)

	connector.Send_to_Peers(bytes)
}
