package dataservers

import (
	"crypto/sha256"
	"encoding/binary"
	// "errors"
)

type Hash [4]uint64

type DataServer struct {
	Hash    Hash
	Address string
	Port    string
}

type DataServers struct {
	Servers []DataServer
	Size    int
}

func compare(hash_a Hash, hash_b Hash) int {

	for index, integer := range hash_a {

		if integer > hash_b[index] {
			return 1
		} else if integer < hash_b[index] {
			return -1
		} 

	}
    return 0
}

// 0 <= index <= len(a)
func (dataservers *DataServers) insert(ds DataServer) {

	value := ds.Hash

	a := &dataservers.Servers

	_, index := dataservers.binarySearch(value)

	if len(*a) == index { // nil or empty slice or after last element
		*a = append(*a, ds)
	}
	*a = append((*a)[:index+1], (*a)[index:]...) // index < len(a)
	(*a)[index] = ds
}

// this may return a value outside the list
// if hash exists in the list returns l as previous position and r as the position of the element
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

	uint64

	hash := sha256.New()

	hash.Write([]byte(ip))
	hash.Write([]byte(port))

	dataServer := DataServer{
		Hash:    Hash(binary.LittleEndian.Uint64(hash.Sum(nil))),
		Address: ip,
		Port:    port,
	}

	dataServers.insert(dataServer)

	return nil
}
