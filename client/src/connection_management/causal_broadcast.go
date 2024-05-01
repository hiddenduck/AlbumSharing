package connectionmanagement

import (
	"fmt"
	pb "main/connection_management/cbCastProtobuf"
	"sync"

	zmq "github.com/pebbe/zmq4"
	proto "google.golang.org/protobuf/proto"
)

type CausalBroadcastInfo struct {
	messageBuffer    map[*pb.CbCastMessage]struct{}
	mutex            sync.Mutex
	hasVersionVector bool
	self             uint32
	connectorInfo    ConnectorInfo
	versionVector    map[uint32]uint64
	changedNodes     map[uint32]uint64
	replySocket      *zmq.Socket
}

func test_msg(src uint32, self_versionVector *map[uint32]uint64, changedNodes *map[uint32]uint64) (flag bool) {

	flag = (*self_versionVector)[src] == ((*changedNodes)[src] + 1)

	if flag {
		for node, version := range *changedNodes {
			if node == src {
				continue
			}
			//This condition will be verified for new nodes, as the map returns 0 by default
			//This is fine, messages from new nodes will be received in order with no trouble, so we must wait for them
			if version > (*self_versionVector)[node] {
				flag = false
				break
			}
		}
	}
	return
}

func (causalBroadcastInfo *CausalBroadcastInfo) update_versionVector(changedNodes *map[uint32]uint64) {

	for node, version := range *changedNodes {

		_, ok := causalBroadcastInfo.versionVector[node]

		if !ok {
			causalBroadcastInfo.versionVector[node] = version
		}
	}
}

func (causalBroadcastInfo *CausalBroadcastInfo) update_state(changedNodes *map[uint32]uint64, src uint32) {

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

func (causalBroadcastInfo *CausalBroadcastInfo) buffer_messages_loop(ch chan []byte) {

	connector := causalBroadcastInfo.connectorInfo

	causalBroadcastInfo.messageBuffer = make(map[*pb.CbCastMessage]struct{}, 0)

	buffer := causalBroadcastInfo.messageBuffer

	for {
        //TODO this only receives message part
		parts, _ := connector.RouterSocket.RecvMessageBytes(0)

        bytes := parts[2]
        //NOTE this os hacky, three parts will always be sent, id, delimiter, data

		msg := pb.CbCastMessage{}

		causalBroadcastInfo.mutex.Lock()

		hasVersionVector := causalBroadcastInfo.hasVersionVector

		defer causalBroadcastInfo.mutex.Unlock()

		proto.Unmarshal(bytes, &msg)

		buffer[&msg] = struct{}{}

		if hasVersionVector {
			break
		}

	}

	causalBroadcastInfo.test_buffer_messages(ch)
	go causalBroadcastInfo.fwd_message(ch)
}

func (causalBroadcastInfo *CausalBroadcastInfo) test_buffer_messages(ch chan []byte) {

	self_versionVector := causalBroadcastInfo.versionVector

	buffer := causalBroadcastInfo.messageBuffer

	flag_delivered_message := true

	for flag_delivered_message {

		flag_delivered_message = false

		for buffered_msg := range buffer {

			src, changedNodes, data := unpack_msg(buffered_msg)

			if test_msg(src, &self_versionVector, &changedNodes) {

				causalBroadcastInfo.update_state(&changedNodes, src)

				select {
				case ch <- data:
				default:
				}

				delete(buffer, buffered_msg)

				flag_delivered_message = true

				break

			} else if self_versionVector[src] > (changedNodes[src] + 1) {
				//Test if the message is in the past of the last delivered message for that source
				delete(buffer, buffered_msg)

			}
		}
	}
}

func (causalBroadcastInfo *CausalBroadcastInfo) fwd_message(ch chan []byte) {

	connector := causalBroadcastInfo.connectorInfo

	self_versionVector := causalBroadcastInfo.versionVector

	buffer := causalBroadcastInfo.messageBuffer

	for {

		parts, _ := connector.RouterSocket.RecvMessageBytes(0)

        bytes := parts[2]
        //NOTE this os hacky, three parts will always be sent, id, delimiter, data

		msg := pb.CbCastMessage{}

		proto.Unmarshal(bytes, &msg)

		src, changedNodes, data := unpack_msg(&msg)

		if test_msg(src, &self_versionVector, &changedNodes) {

			causalBroadcastInfo.update_state(&changedNodes, src)

			ch <- data

			causalBroadcastInfo.test_buffer_messages(ch)

		} else {
			buffer[&msg] = struct{}{}
		}

		// causalBroadcastInfo.update_versionVector(&changedNodes) //tem que ser feito so no fim

	}

}

func (causalBroadcastInfo *CausalBroadcastInfo) Start_versionVector_server(port string) {

	socket := causalBroadcastInfo.replySocket

	socket.Bind("tcp://*:" + port)

	for {
		// Wait for next request from client
		socket.Recv(0) //NOTE: ignoring messages, only here to block until someone wants it

		//using protobuf, maybe uneccessary
		data := pb.CbCastMessage{
			Src:          causalBroadcastInfo.self,
			ChangedNodes: causalBroadcastInfo.changedNodes,
			Data:         make([]byte, 0),
		}

		bytes, _ := proto.Marshal(&data)

		// Send reply back to client
		socket.SendBytes(bytes, 0)
	}
}

func (causalBroadcastInfo *CausalBroadcastInfo) CausalReceive() {

	ch := make(chan []byte, 1024) //buffered channel for 1024 messages

	context, _ := zmq.NewContext() //NOTE: vou deixar assim por agora mas os gajos do zeroMQ recomendam usar so um context

	requestSocket, _ := context.NewSocket(zmq.REQ)

	go causalBroadcastInfo.buffer_messages_loop(ch)

	requestSocket.Send("", 0) //This bitch blocks

	bytes, _ := requestSocket.RecvBytes(0)

	msg := pb.CbCastMessage{}

	proto.Unmarshal(bytes, &msg)

	_, versionVector, _ := unpack_msg(&msg)

	causalBroadcastInfo.mutex.Lock()

	causalBroadcastInfo.hasVersionVector = true

	causalBroadcastInfo.versionVector = versionVector

	defer causalBroadcastInfo.mutex.Unlock()

	for msg := range ch {
		fmt.Println(string(msg))
	}

}

func InitCausalBroadCast(self uint32) (causalBroadcastInfo CausalBroadcastInfo) {

	context, _ := zmq.NewContext() //NOTE: vou deixar assim por agora mas os gajos do zeroMQ recomendam usar so um context
	//temos que ver depois como encapsular para ter so um context

	replySocket, _ := context.NewSocket(zmq.REQ)

	connectorInfo := Make_ConnectorInfo()

	changedNodes := make(map[uint32]uint64)

	changedNodes[self] = uint64(0)

	causalBroadcastInfo.connectorInfo = connectorInfo
	causalBroadcastInfo.versionVector = make(map[uint32]uint64)
	causalBroadcastInfo.self = self
	causalBroadcastInfo.changedNodes = changedNodes
	causalBroadcastInfo.replySocket = replySocket

	return
}

func (causalBroadcastInfo *CausalBroadcastInfo) CausalBroadcast(msg []byte) {

	data := pb.CbCastMessage{
		Src:          causalBroadcastInfo.self,
		ChangedNodes: causalBroadcastInfo.changedNodes,
		Data:         msg,
	}

	bytes, _ := proto.Marshal(&data)

	causalBroadcastInfo.connectorInfo.Send_to_Peers(bytes)
}
