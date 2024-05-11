package main

import (
	dataservers "main/DataServers"
)

func main() {

	ds := dataservers.InitDataServer()

	ds.AddServer("localhost", "1234")

	dataservers.DownLoadFile(ds, "lmao.txt", dataservers.Hash([]byte("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))

}
