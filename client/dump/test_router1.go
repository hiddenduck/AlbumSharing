package main

import (
	"fmt"
	zmq "github.com/pebbe/zmq4"
)

func dump(sink *zmq.Socket) {
    // for {
    parts, _  := sink.RecvMessage(0)

    for i, x := range parts{
        fmt.Printf("%v: %v\n", i, x)
    }
    fmt.Println("no more")
    // }
}

func main() {
	context, _ := zmq.NewContext()

	sink, err := context.NewSocket(zmq.ROUTER)
	if err != nil {
		print(err)
	}
	defer sink.Close()
    sink.SetIdentity("PEER1")
	sink.Bind("tcp://*:1111")

	// //  Then set the identity ourselves
	// identified, err := context.NewSocket(zmq.REQ)
	// if err != nil {
	// 	print(err)
	// }
	// defer identified.Close()
	// identified.SetIdentity("PEER2")
	// identified.Connect("tcp://localhost:1111")
	// identified.Send("lmao", zmq.DONTWAIT)

	dump(sink)
}

