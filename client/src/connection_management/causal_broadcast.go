package connectionmanagement

import (
	"fmt"
	pb "main/connection_management/cbCastProtobuf"
	"math/rand"
	"sync"

	zmq "github.com/pebbe/zmq4"
	proto "google.golang.org/protobuf/proto"
)

type CausalBroadcastInfo struct {
	messageBuffer    map[*pb.CbCastMessage]struct{}
	mutex            sync.Mutex
	hasVersionVector bool
	self             uint32
	ConnectorInfo    ConnectorInfo
	versionVector    map[uint32]uint64
	changedNodes     map[uint32]uint64
	ReplySocket      *zmq.Socket
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

	fmt.Printf("Started buffer_messages_loop\n")

	connector := causalBroadcastInfo.ConnectorInfo

	causalBroadcastInfo.messageBuffer = make(map[*pb.CbCastMessage]struct{}, 0)

	buffer := causalBroadcastInfo.messageBuffer

	for {
		//TODO this only receives message part
		parts, _ := connector.RouterSocket.RecvMessageBytes(0)

		bytes := parts[2]
		//NOTE this os hacky, three parts will always be sent, id, delimiter, data

		// fmt.Printf("Received message (loop) from socket with bytes: %v\n", bytes)

		msg := pb.CbCastMessage{}

		causalBroadcastInfo.mutex.Lock()
        defer causalBroadcastInfo.mutex.Unlock()

		// fmt.Printf("i done did the lock\n")

		hasVersionVector := causalBroadcastInfo.hasVersionVector

		// fmt.Printf("hasVersionVector: %v\n", hasVersionVector)

        causalBroadcastInfo.mutex.Unlock()

		proto.Unmarshal(bytes, &msg)

		buffer[&msg] = struct{}{}

		if hasVersionVector {
			// fmt.Printf("broke\n")
			break
		}

	}

	causalBroadcastInfo.test_buffer_messages(ch)

	causalBroadcastInfo.fwd_message(ch)
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

	connector := causalBroadcastInfo.ConnectorInfo

	self_versionVector := causalBroadcastInfo.versionVector

	buffer := causalBroadcastInfo.messageBuffer

	for {

		fmt.Printf("waiting on receive\n")

		parts, _ := connector.RouterSocket.RecvMessageBytes(0)

		bytes := parts[2]
		//NOTE this os hacky, three parts will always be sent, id, delimiter, data

		// fmt.Printf("Received message from socket with bytes: %v\n", bytes)

		msg := pb.CbCastMessage{}

		proto.Unmarshal(bytes, &msg)

		src, changedNodes, data := unpack_msg(&msg)

		fmt.Printf("changed nodes are: %v\n", changedNodes)

		if test_msg(src, &self_versionVector, &changedNodes) {

			causalBroadcastInfo.update_state(&changedNodes, src)

			fmt.Printf("delivered message with bytes: %v\n", bytes)

			select {
			case ch <- data:
			default:
			}

			causalBroadcastInfo.test_buffer_messages(ch)

		} else {
			buffer[&msg] = struct{}{}

			fmt.Printf("buffered message with bytes: %v\n", bytes)

		}

		causalBroadcastInfo.update_versionVector(&changedNodes) //tem que ser feito so no fim

		fmt.Printf("My versionVector is: %v\n", causalBroadcastInfo.versionVector)

	}

}

func (causalBroadcastInfo *CausalBroadcastInfo) Start_versionVector_server(port string) {

	socket := causalBroadcastInfo.ReplySocket

	socket.Bind("tcp://*:" + port)

	fmt.Printf("started versionVector server on port %v\n", port)

	for {
		// Wait for next request from client
		socket.Recv(0) //NOTE: ignoring messages, only here to block until someone wants it

		//using protobuf, maybe uneccessary
		data := pb.CbCastMessage{
			Src:          causalBroadcastInfo.self,
			ChangedNodes: causalBroadcastInfo.versionVector,
			Data:         make([]byte, 0),
		}

		bytes, _ := proto.Marshal(&data)

		fmt.Printf("Received request with bytes: %v\n", bytes)

		// Send reply back to client
		socket.SendBytes(bytes, 0)
	}
}

func (causalBroadcastInfo *CausalBroadcastInfo) connect_to_random_peer(port string, requestSocket *zmq.Socket) {

	peers := causalBroadcastInfo.ConnectorInfo.PeerMap

	k := rand.Intn(len(peers))
	var key string

	for key = range peers {
		if k == 0 {
			break
		}
		k--
	}

	peer := peers[key]

	requestSocket.Connect("tcp://" + peer.Ip_Addres + ":" + port)

	fmt.Printf("Connected to peer: %v\n", peer)

}

func (causalBroadcastInfo *CausalBroadcastInfo) CausalReceive(is_first bool, port string) {

	ch := make(chan []byte, 1024) //buffered channel for 1024 messages

	if is_first {
		causalBroadcastInfo.versionVector = make(map[uint32]uint64)
		causalBroadcastInfo.versionVector[causalBroadcastInfo.self] = 0

		go causalBroadcastInfo.fwd_message(ch)
	} else {

		context, _ := zmq.NewContext() //NOTE: vou deixar assim por agora mas os gajos do zeroMQ recomendam usar so um context

		requestSocket, _ := context.NewSocket(zmq.REQ)

		causalBroadcastInfo.connect_to_random_peer(port, requestSocket)

		fmt.Printf("buffering all messages\n")

		go causalBroadcastInfo.buffer_messages_loop(ch)

		requestSocket.Send("", 0) //This bitch blocks

		fmt.Printf("Sent request\n")

		bytes, _ := requestSocket.RecvBytes(0)

		msg := pb.CbCastMessage{}

		proto.Unmarshal(bytes, &msg)

		_, versionVector, _ := unpack_msg(&msg)

		fmt.Printf("received version vector: %v \n", versionVector)

		causalBroadcastInfo.mutex.Lock()

		defer causalBroadcastInfo.mutex.Unlock()

		causalBroadcastInfo.hasVersionVector = true

		causalBroadcastInfo.versionVector = versionVector

		causalBroadcastInfo.versionVector[causalBroadcastInfo.self] = 0

		fmt.Printf("causalBroadcastInfo.self: %v\n", causalBroadcastInfo.self)

		fmt.Printf("causalBroadcastInfo.versionVector: %v\n", causalBroadcastInfo.versionVector)

		causalBroadcastInfo.mutex.Unlock()

		fmt.Printf("unlocked\n")

	}

	for msg := range ch {
		fmt.Println(string(msg))
	}

}

func InitCausalBroadCast(self uint32) (causalBroadcastInfo CausalBroadcastInfo) {

	context, _ := zmq.NewContext() //NOTE: vou deixar assim por agora mas os gajos do zeroMQ recomendam usar so um context
	//temos que ver depois como encapsular para ter so um context

	replySocket, _ := context.NewSocket(zmq.REP)

	connectorInfo := Make_ConnectorInfo()

	changedNodes := make(map[uint32]uint64)

	changedNodes[self] = uint64(0)

	causalBroadcastInfo.ConnectorInfo = connectorInfo
	causalBroadcastInfo.versionVector = make(map[uint32]uint64)
	causalBroadcastInfo.self = self
	causalBroadcastInfo.changedNodes = changedNodes
	causalBroadcastInfo.ReplySocket = replySocket
	causalBroadcastInfo.messageBuffer = make(map[*pb.CbCastMessage]struct{})

	return
}

func (causalBroadcastInfo *CausalBroadcastInfo) CausalBroadcast(msg []byte) {

    causalBroadcastInfo.mutex.Lock()
    fmt.Printf("Locked\n")
    causalBroadcastInfo.changedNodes[causalBroadcastInfo.self]++

    causalBroadcastInfo.versionVector[causalBroadcastInfo.self]++
    causalBroadcastInfo.mutex.Unlock()
    fmt.Printf("unLocked\n")

    changedNodes := causalBroadcastInfo.changedNodes

	data := pb.CbCastMessage{
		Src:          causalBroadcastInfo.self,
		ChangedNodes: changedNodes,
		Data:         msg,
	}

	bytes, _ := proto.Marshal(&data)

	causalBroadcastInfo.ConnectorInfo.Send_to_Peers(bytes)
}
