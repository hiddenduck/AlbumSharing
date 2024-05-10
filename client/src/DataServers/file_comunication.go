package dataservers

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"log"
	pb "main/DataServers/fileChunkProto"
	"net"
	"os"

	rxgo "github.com/reactivex/rxgo/v2"
	"google.golang.org/grpc"
)

func DataServerReceiver(centralServerConnection net.Conn) {

}

func downloader(dataServer DataServer, ch chan rxgo.Item, fileHash Hash) {

	addr := dataServer.Address + ":" + dataServer.Port

	var opts []grpc.DialOption

	conn, err := grpc.Dial(addr, opts...)

	if err != nil {
		panic(err)
	}

	defer conn.Close()

	client := pb.NewFileClient(conn)

	downloadMessage := &pb.DownloadMessage{
		HashKey: fileHash[:],
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

func producer(ch chan rxgo.Item, fileName string) {

	fd, err := os.Open(fileName)

	if err != nil {
		log.Fatalf("Error to read [file=%v]: %v", fileName, err.Error())
	}
	reader := bufio.NewReader(fd)

	buff := make([]byte, CHUNK_SIZE)

	for {

		bytes_read, err := reader.Read(buff)
		// fmt.Printf("n: %v\n", n)

		if bytes_read != CHUNK_SIZE {
			buff = buff[:bytes_read]
		}

		if bytes_read == 0 {
			if err == nil {
				continue
			}
			if err == io.EOF {
				close(ch)
				break
			}
			log.Fatal(err)
		}

		ch <- rxgo.Of(buff)
	}
	return
}

func UploadFile(dataServer DataServer, fileName string, fileHash Hash) {

	addr := dataServer.Address + ":" + dataServer.Port

	//var opts []grpc.DialOption

	conn, err := grpc.Dial(addr, grpc.WithInsecure())

	//defer conn.Close()

	if err != nil {
		panic(err)
	}

	client := pb.NewFileClient(conn)

	stream, _ := client.Upload(context.Background())

	ch := make(chan rxgo.Item)

	observable := rxgo.FromChannel(ch)

	go producer(ch, fileName)

	<-observable.ForEach(
		func(item interface{}) {

			fileMessage := pb.FileMessage{
				HashKey: fileHash[:],
				Data:    item.([]byte),
			}

			err := stream.Send(&fileMessage)

			if err != nil {
				panic(err)
			}

		},
		func(error error) {
			panic(error)
		},
		func() {
			reply, err := stream.CloseAndRecv()

			if err != nil {
				panic(err)
			}

			fmt.Printf("Uploaded %v lines\n", reply.Lines)
		})

}

/*
func DownLoadFile(dataServers DataServers, fileName string) {

	fd, err := os.Open(fileName)

	if err != nil {
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
*/
