package dataservers

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"log"
	pb "main/DataServers/fileChunkProto"
	"os"
	"os/exec"

	rxgo "github.com/reactivex/rxgo/v2"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func downloader(dataServer DataServer, ch chan rxgo.Item, fileHash Hash) {

	defer close(ch)

	addr := dataServer.Address + ":" + dataServer.Port

	conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()), grpc.WithBlock())

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
	}

}

func uploader(ch chan rxgo.Item, fileName string) {

	defer close(ch)

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
				break
			}
			log.Fatal(err)
		}

		ch <- rxgo.Of(buff)
	}
	return
}

func UploadFile(dataServers DataServers, fileName string) (Hash, error) {

	fileHash, err := HashFile(fileName)

	if err != nil {
		return Hash{}, err
	}

	dataServer := dataServers.FindBucket(fileHash)

	addr := dataServer.Address + ":" + dataServer.Port

	conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()), grpc.WithBlock())

	defer conn.Close()

	if err != nil {
		panic(err)
	}

	client := pb.NewFileClient(conn)

	stream, _ := client.Upload(context.Background())

	ch := make(chan rxgo.Item)

	observable := rxgo.FromChannel(ch)

	go uploader(ch, fileName)

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

			stream.Recv()

		},
		func(error error) {
			panic(error)
		},
		func() {
			fmt.Printf("Uploaded File: %v\n", fileName)

			if err != nil {
				panic(err)
			}
		})
	return fileHash, nil
}

func UploadFile2(dataServers DataServers, fileName string) {
	/*
		fileHash, err := HashFile(fileName)

		if err != nil {
			return Hash{}, err
		}

		dataServer := dataServers.FindBucket(fileHash)
	*/

	os.Chdir("../../../dataServer/ ")

	Cmd := "mvn"
	Args := []string{"exec:java", "-Dexec.mainClass=Main", "-Dexec.args=1311 localhost 8889"} // todo: trocar para comando

	cmd := exec.Command(Cmd, Args...)

	output, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}

	fmt.Printf("%v\n", output)

}

func DownLoadFile(dataServers DataServers, fileName string, fileHash Hash) {

	fd, err := os.Create(fileName)

	defer fd.Close()

	if err != nil {
		panic(err)
	}

	dataServer := dataServers.FindBucket(fileHash)

	ch := make(chan rxgo.Item)

	observable := rxgo.FromChannel(ch)

	go downloader(dataServer, ch, fileHash)

	<-observable.ForEach(
		func(item interface{}) { //OnNext

			fileMessage := item.(*pb.FileMessage)

			_, err := fd.Write(fileMessage.GetData())

			if err != nil {
				fmt.Printf("err: %v\n", err)
			}

		},
		func(error error) { //OneError
			panic(error)
		},
		func() { //OnComplete
			fmt.Printf("Completed Download\n")

		},
	)

}
