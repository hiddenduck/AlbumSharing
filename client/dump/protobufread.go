package main

import (
	pb "client/protobuf"
	"fmt"
	"io/ioutil"
	"log"

	pt "google.golang.org/protobuf/proto"
)

func main() {

	// Read the existing address book.
	in, err := ioutil.ReadFile("file")
	if err != nil {
		log.Fatalln("Error reading file:", err)
	}
	book := &pb.FileMessage{}

	if err := pt.Unmarshal(in, book); err != nil {
		log.Fatalln("Failed to parse address book:", err)
	}

    fmt.Printf("%v,%v\n", book.Command, book.Key)

}
