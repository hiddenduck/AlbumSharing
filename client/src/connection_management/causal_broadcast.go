package connectionmanagement

import (
	"fmt" 
    proto "google.golang.org/protobuf/proto"
	pb "main/connection_management/cbCastProtobuf"
)

type CausalBroadcastInfo struct{

    self uint32
    connectorInfo ConnectorInfo
    versionVector []uint64
}

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

func InitCausalBroadCast() (causalBroadcastInfo CausalBroadcastInfo, self uint32){

    connectorInfo := Make_ConnectorInfo()

    causalBroadcastInfo.connectorInfo = connectorInfo
    causalBroadcastInfo.versionVector = []uint64{}
    causalBroadcastInfo.self = self

    return
}

func (causalBroadcastInfo CausalBroadcastInfo) AddCausalPeer(id uint32, name string, ip string, port string){

    N := uint32(len(causalBroadcastInfo.versionVector))

    if N == id {
        causalBroadcastInfo.versionVector = append(causalBroadcastInfo.versionVector, 0)
    }else if N < id {
        panic("Bad, Bad, Bad")
    }

    causalBroadcastInfo.connectorInfo.Add_Peer(name, ip, port)
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
