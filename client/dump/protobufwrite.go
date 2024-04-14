package main

import (
	pb "client/protobuf"
	pt "google.golang.org/protobuf/proto"
    "log"
    "io/ioutil"
)

func main() {

	p := &pb.FileMessage{
		Command: pb.CommandType_I_HAVE,
		Key:     []byte("lmao"),
		Data: &pb.FileMessage_Data{
			Bytes: []byte("lmao"),
			End:   false,
		},
	}

    out, err := pt.Marshal(p)

	if err != nil {
		log.Fatalln("Failed to encode address book:", err)
	}
	if err := ioutil.WriteFile("file", out, 0644); err != nil {
		log.Fatalln("Failed to write address book:", err)
	}

}
