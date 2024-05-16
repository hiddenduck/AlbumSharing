package connectionmanagement

import (
	"sync"
	"time"

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
	Mutex        *sync.Mutex
}

func Make_ConnectorInfo() (connectorInfo ConnectorInfo) {

	context, _ := zmq.NewContext()

	routerSocket, _ := context.NewSocket(zmq.ROUTER)

	connectorInfo.PeerMap = make(map[string]ClientInfo)
	connectorInfo.RouterSocket = routerSocket
	connectorInfo.Mutex = &sync.Mutex{}
	return
}

func (connectorInfo ConnectorInfo) Close() {
	connectorInfo.RouterSocket.Close()
}

func (connectorInfo ConnectorInfo) Add_Peer(id string, name string, ip string, port string) {

	clientInfo := ClientInfo{
		Ip_Addres: ip,
		Port:      port,
		Id:        id,
	}

	connectorInfo.PeerMap[name] = clientInfo
}

// NOTE: isto tem que ser feito antes do bind e dos connects
func (connectorInfo ConnectorInfo) SetIdentity(self_id string) {
	connectorInfo.RouterSocket.SetIdentity(self_id)
}

func (connectorInfo ConnectorInfo) BindSocket(port string) {
	connectorInfo.RouterSocket.Bind("tcp://*:" + port)
}

func (connectorInfo ConnectorInfo) Connect_to_Peers() {

	for _, clientInfo := range connectorInfo.PeerMap {

		ip := clientInfo.Ip_Addres
		port := clientInfo.Port

		connectorInfo.RouterSocket.Connect("tcp://" + ip + ":" + port)
	}
}

func (connectorInfo ConnectorInfo) Sender(s int, id string, msgType string, msg []byte) {

	time.Sleep(time.Duration(s * int(time.Millisecond)))

	connectorInfo.Mutex.Lock()
	defer connectorInfo.Mutex.Unlock()

	connectorInfo.RouterSocket.Send(id, zmq.SNDMORE)
	connectorInfo.RouterSocket.Send(msgType, zmq.SNDMORE)
	connectorInfo.RouterSocket.SendBytes(msg, 0)
}

func (connectorInfo ConnectorInfo) Send_to_Peers(msgType string, msg []byte) (err error) {

	// var s int

	connectorInfo.Mutex.Lock()
	defer connectorInfo.Mutex.Unlock()

	for _, clientInfo := range connectorInfo.PeerMap {

		id := clientInfo.Id

		_, err := connectorInfo.RouterSocket.Send(id, zmq.SNDMORE)
		if err != nil {
			return err
		}
		connectorInfo.RouterSocket.Send(msgType, zmq.SNDMORE)
		connectorInfo.RouterSocket.SendBytes(msg, 0)
	}

	return nil
}

func (ConnectorInfo ConnectorInfo) SetFilter(filter string) {
	ConnectorInfo.RouterSocket.SetSubscribe(filter)
}
