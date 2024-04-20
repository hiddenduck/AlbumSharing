package connectionmanagement

import (
	"fmt"
	proto "google.golang.org/protobuf/proto"
	pb "main/connection_management/cbCastProtobuf"
)

type CausalBroadcastInfo struct {
	self          uint32
	connectorInfo ConnectorInfo
	versionVector map[uint32]uint64
	changedNodes  map[uint32]uint64
}

func (causalBroadcastInfo CausalBroadcastInfo) CausalReceive() {

	ch := make(chan []byte)

	go causalBroadcastInfo.fwd_message(ch)

	for msg := range ch {
		fmt.Println(string(msg))
	}

}

//Devolve 0 se funcionou, negativo se mensagem futura e positivo se mensagem passada
func test_msg(src uint32, self_versionVector *map[uint32]uint64, changedNodes *map[uint32]uint64) uint32 {

	flag := (*self_versionVector)[src] - ((*changedNodes)[src] + 1)

	if flag == 0 {
		for node, version := range *changedNodes {
			if node == src {
				continue
			}
			if version <= (*self_versionVector)[node] {
				flag = -1
				break
			}
		}
	}
	return flag
}

func (causalBroadcastInfo CausalBroadcastInfo) update_versionVector(changedNodes *map[uint32]uint64) {

    for node, version := range *changedNodes{

        _, ok := causalBroadcastInfo.versionVector[node]

        if !ok {
            causalBroadcastInfo.versionVector[node] = version
        }
    }
}

func (causalBroadcastInfo CausalBroadcastInfo) update_state(changedNodes *map[uint32]uint64, src uint32) {

	for node, version := range *changedNodes {
		if (node != causalBroadcastInfo.self) && (causalBroadcastInfo.versionVector[node] == version) {
			delete(causalBroadcastInfo.changedNodes, node)
		}
	}

	causalBroadcastInfo.changedNodes[src] = causalBroadcastInfo.versionVector[src]
	causalBroadcastInfo.versionVector[src] += 1
}

func unpack_msg(msg *pb.CbCastMessage) (src uint32, ch map[uint32]uint64, data []byte) {
	//existe getters do protobuf, nao sei qual a melhor alternativa
	src = msg.Src
	ch = msg.ChangedNodes
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

		src, changedNodes, data := unpack_msg(&msg)

		test := test_msg(src, &self_versionVector, &changedNodes)

		if test == 0 {

			causalBroadcastInfo.update_state(&changedNodes, src)

			ch <- data

			for buffered_msg := range buffer {

				src, changedNodes, data := unpack_msg(buffered_msg)

				test := test_msg(src, &self_versionVector, &changedNodes)

				if test == 0 {

					causalBroadcastInfo.update_state(&changedNodes, src)

					ch <- data //ele vai bloquear aqui devido a GO isto tem que ser tratado

					delete(buffer, &msg)
				} else if test > 0 {
					delete(buffer, &msg)
				}
			}

		} else if test < 0 {
			buffer[&msg] = struct{}{}
		}

        causalBroadcastInfo.update_versionVector(&changedNodes) //tem que ser feito so no fim

	}

}

func InitCausalBroadCast(self uint32) (causalBroadcastInfo CausalBroadcastInfo) {

	connectorInfo := Make_ConnectorInfo()

	changedNodes := make(map[uint32]uint64)

	changedNodes[self] = uint64(0)

	causalBroadcastInfo.connectorInfo = connectorInfo
	causalBroadcastInfo.versionVector = make(map[uint32]uint64)
	causalBroadcastInfo.self = self
	causalBroadcastInfo.changedNodes = changedNodes

	return
}

func (causalBroadcastInfo CausalBroadcastInfo) CausalBroadcast(self uint32, msg []byte) {

	data := pb.CbCastMessage{
		Src:          self,
		ChangedNodes: causalBroadcastInfo.changedNodes,
		Data:         msg,
	}

	bytes, _ := proto.Marshal(&data)

	causalBroadcastInfo.connectorInfo.Send_to_Peers(bytes)
}
