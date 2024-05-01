package main

import (
	zmq "github.com/pebbe/zmq4"
)

func main() {

    context, _ := zmq.NewContext()
	//  Then set the identity ourselves
	identified, err := context.NewSocket(zmq.ROUTER)

	if err != nil {
		print(err)
	}
	defer identified.Close()

	identified.SetIdentity("PEER2")
	identified.Connect("tcp://localhost:1111")
    for {

        // identified.SendMessage("PEER1", "", "lmao")
        identified.Send("PEER1", zmq.SNDMORE)
        identified.Send("", zmq.SNDMORE)
        identified.Send("lmao", 0)
    }

}
