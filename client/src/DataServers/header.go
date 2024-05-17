package dataservers

import "sync"

const (
	CHUNK_SIZE = 4096
)

type Hash [32]byte

type DataServer struct {
	Hash    Hash
	Address string
	Port    string
}

type DataServers struct {
	Servers []DataServer
	Size    int
	Mutex   *sync.Mutex
}
