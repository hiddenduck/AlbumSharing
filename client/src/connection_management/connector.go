package connectionmanagement

import (
	"fmt"
	zmq "github.com/pebbe/zmq4"
)

type ClientInfo struct {
	Ip_Addres string
	Port      string
}

type ConnectorInfo struct{
    PeerMap map[string]ClientInfo
    PubSocket *zmq.Socket
    SubSocket *zmq.Socket
}

func Make_ConnectorInfo() ConnectorInfo {

	context, _ := zmq.NewContext()

    pubSocket, _ := context.NewSocket(zmq.PUB)
    subSocket, _ := context.NewSocket(zmq.SUB)

    var connectorInfo ConnectorInfo

    connectorInfo.PeerMap = make(map[string]ClientInfo)
    connectorInfo.PubSocket = pubSocket
    connectorInfo.SubSocket = subSocket

	return connectorInfo
}

func (connectorInfo ConnectorInfo) Add_Peer (name string, ip string, port string) {

	var clientInfo ClientInfo

	clientInfo.Ip_Addres = ip
	clientInfo.Port = port

	connectorInfo.PeerMap[name] = clientInfo
}

func (connectorInfo ConnectorInfo) Start_Publish(port string){
	connectorInfo.PubSocket.Bind("tcp://*:" + port)
}

func (connectorInfo ConnectorInfo) Connect_to_Peers (filter string){

    for _, clientInfo := range connectorInfo.PeerMap{

        ip := clientInfo.Ip_Addres
        port := clientInfo.Port

        connectorInfo.SubSocket.Connect("tcp://" + ip + ":" + port)
        connectorInfo.SubSocket.SetSubscribe(filter)
    }
}

func (connectorInfo ConnectorInfo) Send_to_Peers(msg string) {
    connectorInfo.PubSocket.Send(msg, 0)
}

func (connectorInfo ConnectorInfo) Listen_to_Peers() {
    for {
        msg, _ := connectorInfo.SubSocket.Recv(0)
        fmt.Println(msg)
    }
}

func (connectorInfo ConnectorInfo) Set_Subscribe(filter string) {
    connectorInfo.SubSocket.SetSubscribe(filter)
}
