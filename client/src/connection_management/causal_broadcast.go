package connectionmanagement

import (
	"fmt"
	proto "google.golang.org/protobuf/proto"
	pb "main/connection_management/cbCastProtobuf"
)

type CausalBroadcastInfo struct {
	self          uint32
	connectorInfo ConnectorInfo
	versionVector []uint64
}

func (causalBroadcastInfo CausalBroadcastInfo) CausalReceive() {

	ch := make(chan []byte)

	go causalBroadcastInfo.fwd_message(ch)

	for msg := range ch {
		fmt.Println(string(msg))
	}

}

func updateVersionVector(versionVector *[]uint64, self_versionVector *[]uint64) {
	N := len(*self_versionVector)
	*self_versionVector = append(*self_versionVector, (*versionVector)[N:]...)
}

func test_msg(src uint32, versionVector *[]uint64, self_versionVector *[]uint64, data []byte) bool {

	flag := (*versionVector)[src] == ((*self_versionVector)[src] + 1)

	if !flag {
		return flag
	} else {
        flag = true
		for index := range *versionVector {
			if uint32(index) == src {
				continue
			}
            flag = flag && ((*self_versionVector)[index] <= (*versionVector)[index])
		}
	}
    return flag
}

func unpack_msg(msg *pb.CbCastMessage) (src uint32, vv []uint64, data []byte) {
	//existe getters do protobuf, nao sei qual a melhor alternativa
	src = msg.Src
	vv = msg.VersionVector
	data = msg.Data
	return
}

func (causalBroadcastInfo CausalBroadcastInfo) fwd_message(ch chan []byte) {

	connector := causalBroadcastInfo.connectorInfo

	self_versionVector := causalBroadcastInfo.versionVector

	//this is a set
	var buffer = make(map[*pb.CbCastMessage]struct{}, 0)

	for {

		bytes, _ := connector.SubSocket.RecvBytes(0)

		msg := pb.CbCastMessage{}

		proto.Unmarshal(bytes, &msg)

		src, versionVector, data := unpack_msg(&msg)

		if len(versionVector) > len(self_versionVector) {
			updateVersionVector(&versionVector, &self_versionVector)
		}

		if test_msg(src, &versionVector, &self_versionVector, data) {

			ch <- data

			for buffered_msg := range buffer {

				src, versionVector, data = unpack_msg(buffered_msg)

				if test_msg(src, &versionVector, &self_versionVector, data) {
					ch <- data
					delete(buffer, &msg)
				}
			}

		} else {
			buffer[&msg] = struct{}{}
		}
	}

}

func InitCausalBroadCast() (causalBroadcastInfo CausalBroadcastInfo, self uint32) {

	connectorInfo := Make_ConnectorInfo()

	causalBroadcastInfo.connectorInfo = connectorInfo
	causalBroadcastInfo.versionVector = []uint64{}
	causalBroadcastInfo.self = self

	return
}

func (causalBroadcastInfo CausalBroadcastInfo) CausalBroadcast(self uint32, msg []byte, versionVector []uint64) {

	connector := causalBroadcastInfo.connectorInfo

	data := pb.CbCastMessage{
		Src:           self,
		VersionVector: versionVector,
		Data:          msg,
	}

	bytes, _ := proto.Marshal(&data)

	connector.Send_to_Peers(bytes)
}
