package chat

import (
	"fmt"
	"log"
	"time"

	"github.com/pebbe/zmq4"
	zmq "github.com/pebbe/zmq4"
)

type ClientInfo struct {
    Ip_Addres string
    Port string
}

type ClientMap map[string]ClientInfo

func Make_ClientMap() (ClientMap) {
    return make(map[string]ClientInfo)
}

func AddTo_ClientMap(clientMap ClientMap, name string, ip string, port string){

    var clientInfo ClientInfo
    clientInfo.Ip_Addres = ip
    clientInfo.Port = port

    clientMap[name] = clientInfo
}

func Server(port string) {
	zctx, _ := zmq.NewContext()

	s, _ := zctx.NewSocket(zmq.REP)
	s.Bind("tcp://*:" + port)

	for {
		// Wait for next request from client
		msg, _ := s.Recv(0)
		log.Printf("Received %s\n", msg)

		// Do some 'work'
		time.Sleep(time.Second * 1)

		// Send reply back to client
		s.Send("World", 0)
	}
}

func client(ip string, port string) (*zmq4.Socket) {

	zctx, _ := zmq.NewContext()

	// Socket to talk to server
	fmt.Printf("Connecting to the server...\n")
	s, _ := zctx.NewSocket(zmq.REQ)
	s.Connect("tcp://"+ip+":"+port)

    return s

	// Do 10 requests, waiting each time for a response
	// for i := 0; i < 10; i++ {
	// 	fmt.Printf("Sending request %d...\n", i)
	// 	s.Send("Hello", 0)
	//
	// 	msg, _ := s.Recv(0)
	// 	fmt.Printf("Received reply %d [ %s ]\n", i, msg)
	// }
}

func SendToAll(clients ClientMap, message string)  {
    for name, clientInfo := range clients{
    
        ip := clientInfo.Ip_Addres
        port := clientInfo.Port

        socket := client(string(ip), port)

        socket.Send(string(name)+": "+message, 0)

    }
}
