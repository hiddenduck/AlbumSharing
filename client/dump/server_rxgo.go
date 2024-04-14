package main

import (
	"context"
	"fmt"
	"net"

	proto "google.golang.org/protobuf/proto"
	pb "main/protobuf"
	// zmq "github.com/pebbe/zmq4"
	rxgo "github.com/reactivex/rxgo/v2"
)

const (
	SERVER_HOST = "localhost"
	SERVER_PORT = "1234"
	SERVER_TYPE = "tcp"
)

func producer(ch chan rxgo.Item, connection net.Conn) {

	for {

		buffer := make([]byte, 1024)
		mLen, err := connection.Read(buffer)

		p := pb.FileMessage{}
		proto.Unmarshal(buffer, &p)

		fmt.Println("Received: ", string(buffer[:mLen]))
		ch <- rxgo.Of(p)

		if err != nil {
			close(ch)
			fmt.Println("Error reading:", err.Error())
			break
		}

	}
}

func main() {

	listener, err := net.Listen(SERVER_TYPE, ":"+SERVER_PORT)

	if err != nil {
		println("lmao")
		panic(err)
	}

	defer listener.Close()

	connection, err := listener.Accept()

	defer connection.Close()

	if err != nil {
		println("lmao")
		panic(err)
	}

	// Create the input channel
	ch := make(chan rxgo.Item)

	// Create an Observable
	observable := rxgo.FromChannel(ch)

	// Data producer
	go producer(ch, connection)

	observable = observable.Map(func(_ context.Context, item interface{}) (interface{}, error) {
		fileMessage := item.(pb.FileMessage)

		return fileMessage, nil

	}).Map(func(_ context.Context, item interface{}) (interface{}, error) {
		fmt.Println("lmao")
		return item, nil
	})
	// .DoOnNext(func (item interface{}){print("iduno")})
	// observable.DoOnCompleted(func () {print("DoOnCompleted")})

	<-observable.ForEach(
		func(item interface{}) { fmt.Println(item) },
		func(error error) {
			panic(error)
		},
		func() {
			println("i coomed")
		})

}
