package main

import (
	"bufio"
	"context"
	"crypto/sha256"
	"fmt"
	"io"
	"log"
	"os"

	// zmq "github.com/pebbe/zmq4"
	rxgo "github.com/reactivex/rxgo/v2"
)

func producer(ch chan rxgo.Item) (nBytes int64, nChunks int64) {

    const N int = 10485940/4
    // const N int = 10737418240



	fileName := os.Args[1]
	f, err := os.Open(fileName)

	if err != nil {
		log.Fatalf("Error to read [file=%v]: %v", fileName, err.Error())
	}
	r := bufio.NewReader(f)
	buf := make([]byte, N)

	for {

        n, err := r.Read(buf)
        // fmt.Printf("n: %v\n", n)
		buf = buf[:n]

		if n == 0 {
			if err == nil {
				continue
			}
			if err == io.EOF {
                close(ch)
				break
			}
			log.Fatal(err)
		}

		nChunks++
		nBytes += int64(len(buf))

		// process buf
		if err != nil && err != io.EOF {
			log.Fatal(err)
		}

        // fmt.Printf("buf: %v\n", buf)

        buff_copy := make([]byte, n)

        copy(buff_copy, buf)

        // fmt.Printf("buff_copy: %v\n", buff_copy)

        ch<-rxgo.Of(buff_copy)
	}
    fmt.Printf("nChunks: %v\n", nChunks)
    fmt.Printf("nBytes: %v\n", nBytes)
    return
}

func main() {

    h := sha256.New()

	// Create the input channel
	ch := make(chan rxgo.Item)

	// Create an Observable
	observable := rxgo.FromChannel(ch)

	// Data producer
	go producer(ch)

	observable = observable.Map(func(_ context.Context, item interface{}) (interface{}, error) {

		buff := item

        h.Write(buff.([]byte))

		return buff, nil

	})

	<-observable.ForEach(

		func(item interface{}) {},

		func(error error) {
			panic(error)
		},
		func() {
            fmt.Printf("h.Sum(nil): %v\n", h.Sum(nil))
			println("i coomed")
		})

}



