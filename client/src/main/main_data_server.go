package main

import (
	"fmt"
	ds "main/DataServers"
)

func main() {

	dataservers := ds.InitDataServer()

	dataservers.AddServer("localhost", "1236")
	dataservers.AddServer("localhost", "1237")
	dataservers.AddServer("localhost", "1234")
	dataservers.AddServer("localhost", "1235")

	for _, x := range dataservers.Servers {
		fmt.Printf("x.Hash: %v\n", x.Hash)
	}

	h := ds.Hash([]byte{254, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})

    bucket := dataservers.FindBucket(h)

    fmt.Printf("bucket: %v\n", bucket)


}
