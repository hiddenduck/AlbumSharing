package connectionmanagement

import (
	"fmt"
	pb "main/connection_management/cbCastProtobuf"
	"math/rand"
	"time"

	// "time"

	zmq "github.com/pebbe/zmq4"
	proto "google.golang.org/protobuf/proto"
)

type ChannelWriteMessage struct {
	Message []byte
	Peer    string
}

type CausalMessage struct {
	Message *pb.CbCastMessage
	Peer    string
}

type CausalBroadcastInfo struct {
	VVrequestBuffer map[string]bool
	messageBuffer   map[CausalMessage]struct{}
	self            uint32
	ConnectorInfo   *ConnectorInfo
	versionVector   map[uint32]uint64
	changedNodes    map[uint32]uint64
	ReplySocket     *zmq.Socket
	Channel         chan ChannelWriteMessage
}

func (causalBroadcastInfo *CausalBroadcastInfo) PrintVV() {
	fmt.Printf("VV: %+v\n", causalBroadcastInfo.versionVector)
}

func test_msg(src uint32, self_versionVector *map[uint32]uint64, changedNodes *map[uint32]uint64) (flag bool) {

	flag = ((*self_versionVector)[src] + 1) == (*changedNodes)[src]

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

	causalBroadcastInfo.versionVector[src] += 1
	causalBroadcastInfo.changedNodes[src] = causalBroadcastInfo.versionVector[src]
}

func unpack_msg(msg *pb.CbCastMessage) (src uint32, ch map[uint32]uint64, data []byte) {
	//existe getters do protobuf, nao sei qual a melhor alternativa
	src = msg.Src
	ch = msg.ChangedNodes
	data = msg.Data
	return
}

func (causalBroadcastInfo *CausalBroadcastInfo) Buffer_message(input []string) {

	//fmt.Printf("Buffering message\n")

	buffer := causalBroadcastInfo.messageBuffer
	//NOTE this os hacky, three parts will always be sent, id, delimiter, data

	// fmt.Printf("Received message (loop) from socket with bytes: %v\n", bytes)

	msg := pb.CbCastMessage{}

	proto.Unmarshal([]byte(input[2]), &msg)

	buffer[CausalMessage{&msg, input[0]}] = struct{}{}
}

func (causalBroadcastInfo *CausalBroadcastInfo) Test_buffer_messages() {

	self_versionVector := causalBroadcastInfo.versionVector

	buffer := causalBroadcastInfo.messageBuffer

	flag_delivered_message := true

	//fmt.Printf("Testing buffered messages\n")

	for flag_delivered_message {

		flag_delivered_message = false

		for buffered_msg := range buffer {

			//fmt.Printf("Testing (my versionVector) %v\n", causalBroadcastInfo.versionVector)

			src, changedNodes, data := unpack_msg(buffered_msg.Message)

			//fmt.Printf("Testing (changedNodes) %v\n", changedNodes)

			if test_msg(src, &self_versionVector, &changedNodes) {

				causalBroadcastInfo.update_state(&changedNodes, src)

				//fmt.Printf("Delivered a buffered message\n")

				select {
				case causalBroadcastInfo.Channel <- ChannelWriteMessage{data, buffered_msg.Peer}:
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

func (causalBroadcastInfo *CausalBroadcastInfo) Fwd_message(input []string) {

	self_versionVector := causalBroadcastInfo.versionVector

	buffer := causalBroadcastInfo.messageBuffer

	//NOTE this os hacky, three parts will always be sent, id, delimiter, data

	//fmt.Printf("Received message")

	msg := pb.CbCastMessage{}

	peer := input[0]
	bytes := []byte(input[2])

	proto.Unmarshal(bytes, &msg)

	//fmt.Printf("My previous versionVector is: %v\n", causalBroadcastInfo.versionVector)

	src, changedNodes, data := unpack_msg(&msg)

	//fmt.Printf("changed nodes are: %v\n", changedNodes)

	if test_msg(src, &self_versionVector, &changedNodes) {

		causalBroadcastInfo.update_state(&changedNodes, src)

		//fmt.Printf("delivered message with bytes: %v\n", bytes)

		select {
		case causalBroadcastInfo.Channel <- ChannelWriteMessage{data, peer}:
		default:
		}

		causalBroadcastInfo.Test_buffer_messages()

	} else {
		buffer[CausalMessage{&msg, peer}] = struct{}{}

		//fmt.Printf("buffered message with bytes: %v\n", bytes)

	}

	// causalBroadcastInfo.update_versionVector(&changedNodes) //tem que ser feito so no fim

	//fmt.Printf("My updated versionVector is: %v\n", causalBroadcastInfo.versionVector)

}

func (causalBroadcastInfo *CausalBroadcastInfo) SendVersionVector(id string) {
	//using protobuf, maybe uneccessary
	data := pb.CbCastMessage{
		Src:          causalBroadcastInfo.self,
		ChangedNodes: causalBroadcastInfo.versionVector,
		Data:         make([]byte, 0),
	}

	bytes, _ := proto.Marshal(&data)

	fmt.Printf("Received request, sending VV: %v\n", data.ChangedNodes)

	// Send reply back to client
	causalBroadcastInfo.ConnectorInfo.Sender(1, id, "replyVV", bytes)
}

func (causalBroadcastInfo *CausalBroadcastInfo) AnswerVVRequests() {
	for peer := range causalBroadcastInfo.VVrequestBuffer {
		causalBroadcastInfo.SendVersionVector(peer)
	}
	for k := range causalBroadcastInfo.VVrequestBuffer {
		delete(causalBroadcastInfo.VVrequestBuffer, k)
	}

}

func (causalBroadcastInfo *CausalBroadcastInfo) AddVVRequest(peer string) {
	causalBroadcastInfo.VVrequestBuffer[peer] = true
}

func (causalBroadcastInfo *CausalBroadcastInfo) RequestVV() {

	peers := causalBroadcastInfo.ConnectorInfo.PeerMap

	k := rand.Intn(len(peers))
	var key string

	for key = range peers {
		if k == 0 {
			break
		}
		k--
	}
	randomId := peers[key].Id

	causalBroadcastInfo.ConnectorInfo.Sender(1, randomId, "requestVV", []byte("control")) //This bitch blocks

	fmt.Printf("Sent VV request to node %v\n", randomId)
}

func (causalBroadcastInfo *CausalBroadcastInfo) CausalReceive(is_first bool) {

	if is_first {
		causalBroadcastInfo.versionVector = make(map[uint32]uint64)
		causalBroadcastInfo.versionVector[causalBroadcastInfo.self] = 0

	} else {
		//time.Sleep(5 * time.Second)
		causalBroadcastInfo.RequestVV()
	}

	for msg := range causalBroadcastInfo.Channel {
		fmt.Printf("%v: %v\n", msg.Peer, string(msg.Message))
	}

}

func (causalBroadcastInfo *CausalBroadcastInfo) ReceiveVV(bytes []byte) {
	time.Sleep(10 * time.Second)

	msg := pb.CbCastMessage{}

	proto.Unmarshal(bytes, &msg)

	_, versionVector, _ := unpack_msg(&msg)

	//fmt.Printf("received version vector: %v \n", versionVector)

	versionVector[causalBroadcastInfo.self] = causalBroadcastInfo.versionVector[causalBroadcastInfo.self]

	causalBroadcastInfo.versionVector = versionVector

	//causalBroadcastInfo.hasVersionVector = true

	causalBroadcastInfo.AnswerVVRequests()

	causalBroadcastInfo.Test_buffer_messages()

	//fmt.Printf("causalBroadcastInfo.self: %v\n", causalBroadcastInfo.self)

	//fmt.Printf("causalBroadcastInfo.versionVector: %v\n", causalBroadcastInfo.versionVector)

	//fmt.Printf("unlocked\n")
}

func InitCausalBroadCast(self uint32, connector *ConnectorInfo) (causalBroadcastInfo CausalBroadcastInfo) {

	//context, _ := zmq.NewContext() //NOTE: vou deixar assim por agora mas os gajos do zeroMQ recomendam usar so um context
	//temos que ver depois como encapsular para ter so um context

	changedNodes := make(map[uint32]uint64)

	changedNodes[self] = uint64(0)

	causalBroadcastInfo.ConnectorInfo = connector
	causalBroadcastInfo.versionVector = make(map[uint32]uint64)
	causalBroadcastInfo.self = self
	causalBroadcastInfo.changedNodes = changedNodes
	causalBroadcastInfo.messageBuffer = make(map[CausalMessage]struct{})
	causalBroadcastInfo.Channel = make(chan ChannelWriteMessage, 1024)
	causalBroadcastInfo.VVrequestBuffer = make(map[string]bool)

	return
}

func (causalBroadcastInfo *CausalBroadcastInfo) incrementVV() {

	causalBroadcastInfo.changedNodes[causalBroadcastInfo.self]++
	causalBroadcastInfo.versionVector[causalBroadcastInfo.self]++
}

func (causalBroadcastInfo *CausalBroadcastInfo) CausalBroadcast(msg []byte) {

	causalBroadcastInfo.incrementVV()

	changedNodes := causalBroadcastInfo.changedNodes

	data := pb.CbCastMessage{
		Src:          causalBroadcastInfo.self,
		ChangedNodes: changedNodes,
		Data:         msg,
	}

	bytes, _ := proto.Marshal(&data)

	//fmt.Printf("Sending changedNodes: %v\n", changedNodes)
	causalBroadcastInfo.ConnectorInfo.Send_to_Peers("chat", bytes)
}
