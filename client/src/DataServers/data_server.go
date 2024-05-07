package dataservers

import (
	"crypto/sha256"
	"errors"
)

type Hash int32

type DataServer struct {
	Hash    Hash
	Address string
	Port    string
}

type DataServers struct {
	Servers []DataServer
	Size    int
}

func (dataServers *DataServers) binarySearch(hash Hash) {

	l, r := 0, dataServers.Size

	for r != l {

		m := (l + r) / 2

        if dataServers.Servers[m].Hash > hash {

            r = m - 1

        }

	}

}

// TODO check if server exists in the list
// kinda dont care is this is slow (will not be used alot so who cares)
func (dataServers *DataServers) AddServer(ip string, port string) error {

	hash := sha256.New()

	hash.Write([]byte(ip))
	hash.Write([]byte(port))

	dataServer := DataServer{
		Hash:    Hash(hash.Sum(nil)),
		Address: ip,
		Port:    port,
	}

	dataServers.Servers = append(dataServers.Servers, dataServer)

	return nil
}
