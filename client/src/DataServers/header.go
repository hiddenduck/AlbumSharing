package dataservers

const(
    CHUNK_SIZE = 10485940
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
}
