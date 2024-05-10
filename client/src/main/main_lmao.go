package main

import (
	dataservers "main/DataServers"
)

func main() {
	ds := dataservers.InitDataServer()
	ds.AddServer("localhost", "1234")
	h := dataservers.Hash([]byte{254, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})
	dataservers.UploadFile(ds.Servers[0], "lmao.txt", h)
}
