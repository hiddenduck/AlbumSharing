package crdt

import "fmt"

var currentID uint32 // TODO: get this dude from central server

type Nil struct{}

type Replica struct {
	// filename -> {userName -> rating}
	// DotMap<String, ORSet<(string, int)>>
	// DotMap<String, GSet<(string, int)>> visto que nao se pode mudar o rating
	Files         map[string]map[string]uint8
	Peers         map[string]Nil
	VersionVector map[uint32]uint64
}

func (replica Replica) AddFile(fileName string) bool {

	_, ok := replica.Files[fileName]
	if !ok {
		replica.Files[fileName] = make(map[string]uint8)
		replica.VersionVector[currentID]++

		//TODO: talk with data server
	}

	return !ok
}

func (replica Replica) RemoveFile(fileName string) (ok bool) {

	_, ok = replica.Files[fileName]
	if ok {
		delete(replica.Files, fileName)
	}

	return
}

func (replica Replica) ListFiles() {
	for fileName := range replica.Files {
		fmt.Println(fileName)
	}
}

func (replica Replica) AddUser(userName string) bool {

	_, ok := replica.Peers[userName]
	if !ok {
		replica.Peers[userName] = Nil{}
	}

	return !ok
}

func (replica Replica) RemoveUser(userName string) (ok bool) {
	_, ok = replica.Peers[userName]
	if ok {
		delete(replica.Peers, userName)
	}

	return
}

func (replica Replica) AddUserClassification(userName string, classification uint8) {

}
