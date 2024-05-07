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

//this may return a value outside the list
//if hash exists in the list returns l as previous position and r as the position of the element
func (dataservers *DataServers) binarySearch(hash Hash) (l int, r int) {

    list := dataservers.Servers

	var last = len(list) - 1

	if list[0].Hash >= hash {
		return -1, 0
	}

    if list[last].Hash < hash {
        return last, last + 1
    }

	l, r = 0, last

	var m int

	for r-l > 1 {

        val := list[m].Hash

		m = (l + r) / 2

		if val >= hash {
			r = m
		} else if val < hash {
			l = m
		}

	}

	return
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
