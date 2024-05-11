package main

import (
	"crypto/sha256"
	// "errors"
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

func compare(hash_a Hash, hash_b Hash) int {

	for index := range hash_a {

		if hash_a[index] > hash_b[index] {
			return 1
		} else if hash_a[index] < hash_b[index] {
			return -1
		}
	}
	return 0
}

// 0 <= index <= len(a)
func (dataservers *DataServers) insert(ds DataServer) {

	value := ds.Hash

	_, index := dataservers.binarySearch(value)

	if dataservers.Size == index { // nil or empty slice or after last element

		dataservers.Servers = append(dataservers.Servers, ds)

	} else {

		// *a = append((*a)[:index+1], (*a)[index:]) // index < len(a)

		dataservers.Servers = append(dataservers.Servers[:index+1], dataservers.Servers[index:]...)

		dataservers.Servers[index] = ds

	}

	dataservers.Size += 1
}

// this may return a value outside the list
// if hash exists in the list returns l as previous position and r as the position of the element
func (dataservers *DataServers) binarySearch(hash Hash) (l int, r int) {

	list := dataservers.Servers

	var last = dataservers.Size - 1

	if last < 0 {
		return -1, 0
	}

	if compare(list[0].Hash, hash) >= 0 {
		return -1, 0
	}

	if compare(list[last].Hash, hash) < 0 {
		return last, last + 1
	}

	l, r = 0, last

	var m int

	for r-l > 1 {

		m = (l + r) / 2

		val := list[m].Hash

		if compare(val, hash) >= 0 {
			r = m
		} else if compare(val, hash) < 0 {
			l = m
		}
	}

	return
}

func InitDataServer() DataServers {
	return DataServers{
		Servers: make([]DataServer, 0),
		Size:    0,
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

	dataServers.insert(dataServer)

	return nil
}

func (dataservers *DataServers) FindBucket(hash Hash) DataServer {

	_, r := dataservers.binarySearch(hash)

	return dataservers.Servers[r%dataservers.Size]
}
