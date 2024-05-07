package main

import (
	"bufio"
	"crypto/sha256"
	"fmt"
	"io"
	"log"
	"os"
)


func main() {

    const N int = 10485940

    h := sha256.New()

    chunkSize := 0

	fileName := os.Args[1]
	f, err := os.Open(fileName)

	if err != nil {
		log.Fatalf("Error to read [file=%v]: %v", fileName, err.Error())
	}
	r := bufio.NewReader(f)
	buf := make([]byte, N)

	for {

        n, err := r.Read(buf[:N])
        // fmt.Printf("n: %v\n", n)
		buf = buf[:n]

		if n == 0 {
			if err == nil {
				continue
			}
			if err == io.EOF {
				break
			}
			log.Fatal(err)
		}

		chunkSize++

		// process buf
		if err != nil && err != io.EOF {
			log.Fatal(err)
		}

        // fmt.Printf("buf: %v\n", buf)

        // buff_copy := make([]byte, n)

        // copy(buff_copy, buf)

        // fmt.Printf("buff_copy: %v\n", buff_copy)

        h.Write(buf)

        // ch<-rxgo.Of(buff_copy)
	}

    fmt.Printf("chunkSize: %v\n", chunkSize)

    fmt.Printf("h.Sum(nil): %v\n", h.Sum(nil))


}



