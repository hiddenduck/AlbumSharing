package connectionmanagement

import (
	"fmt"
	zmq "github.com/pebbe/zmq4"
)

type ClientInfo struct {
	Ip_Addres string
	Port      string
	Id        string
}

type ConnectorInfo struct {
	PeerMap      map[string]ClientInfo
	RouterSocket *zmq.Socket
	MessageHandlers map[string]interface{}
}

func myprint(msg []string) {
	for i, x := range msg {
		fmt.Printf("frame %v: %v\n", i, x)
	}
}

func createMessageHandlers() map[string]interface{} {
	return map[string]interface{}{
		"print": myprint,
	}
}

func Make_ConnectorInfo() (connectorInfo ConnectorInfo) {

	context, _ := zmq.NewContext()

	routerSocket, _ := context.NewSocket(zmq.ROUTER)

	connectorInfo.PeerMap = make(map[string]ClientInfo)
	connectorInfo.RouterSocket = routerSocket
	connectorInfo.MessageHandlers = createMessageHandlers()
	return
}

func (connectorInfo ConnectorInfo) Add_Peer(id string, name string, ip string, port string) {

    clientInfo := ClientInfo{
        Ip_Addres: ip,
        Port: port,
        Id: id,
    }

	connectorInfo.PeerMap[name] = clientInfo
}

//NOTE: isto tem que ser feito antes do bind e dos connects
func (connectorInfo ConnectorInfo) SetIdentity(self_id string) {
    connectorInfo.RouterSocket.SetIdentity(self_id)
}

func (connectorInfo ConnectorInfo) BindSocket( port string) {
	connectorInfo.RouterSocket.Bind("tcp://*:" + port)
}

func (connectorInfo ConnectorInfo) Connect_to_Peers() {

	for _, clientInfo := range connectorInfo.PeerMap {

		ip := clientInfo.Ip_Addres
		port := clientInfo.Port

		connectorInfo.RouterSocket.Connect("tcp://" + ip + ":" + port)
	}
}

func (connectorInfo ConnectorInfo) Send_to_Peers(msg []byte) {

    for _, clientInfo := range connectorInfo.PeerMap {

        id := clientInfo.Id

        connectorInfo.RouterSocket.Send(id, zmq.SNDMORE)
        connectorInfo.RouterSocket.Send("print", zmq.SNDMORE)
        connectorInfo.RouterSocket.SendBytes(msg, 0)
    }

}

func (ConnectorInfo ConnectorInfo) SetFilter(filter string){
    ConnectorInfo.RouterSocket.SetSubscribe(filter)
}

func (connectorInfo ConnectorInfo) Listen_to_Peers() {
	for {
		msg, _ := connectorInfo.RouterSocket.RecvMessage(0)

		function, ok := connectorInfo.MessageHandlers[msg[1]]

		if ok {
			function.(func([]string))(msg)
		} else {
			fmt.Printf("\"%v\"; not a valid type of message!\n", msg[1])
		}
	}
}
