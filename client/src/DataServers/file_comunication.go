package dataservers

import (
	"context"
	"fmt"
	"io"
	"log"
	pb "main/DataServers/fileChunkProto"
	"main/crdt"
	"net"
	"os"

	rxgo "github.com/reactivex/rxgo/v2"
	"google.golang.org/grpc"
)

func DataServerReceiver(centralServerConnection net.Conn) {

}

func downloader(dataServer DataServer, ch chan rxgo.Item, fileHash []byte) {

	addr := dataServer.Address + ":" + dataServer.Port

	var opts []grpc.DialOption

	conn, err := grpc.Dial(addr, opts...)

	defer conn.Close()

	if err != nil {
		panic(err)
	}

	client := pb.NewFileClient(conn)

	downloadMessage := &pb.DownloadMessage{
		HashKey: fileHash,
	} // initialize a downloadMessage

	stream, err := client.Download(context.Background(), downloadMessage)

	for {

		chunk, err := stream.Recv()

		if err == io.EOF {
			break
		}

		if err != nil {
			log.Fatalf("%v.ListFeatures(_) = _, %v", client, err)
		}

        ch <- rxgo.Of(chunk)

        defer close(ch)

	}

}

func DownLoadFile(dataServers DataServers, fileName string) {

    fd, err := os.Open(fileName)

    if err != nil{
        panic(err)
    }

	fileHash := crdt.GetFileHash(fileName)

	dataServer := dataServers.FindBucket(fileHash)

	ch := make(chan rxgo.Item)

	observable := rxgo.FromChannel(ch)

	go downloader(dataServer, ch, fileHash)

	<-observable.ForEach(
		func(item interface{}) { //OnNext

            fileMessage := item.(*pb.FileMessage)

            fd.Write(fileMessage.GetData())

		},
		func(error error) { //OneError
			panic(error)
		},
		func() { //OnComplete
            fmt.Printf("Conpleted Download")

		},
	)

}
