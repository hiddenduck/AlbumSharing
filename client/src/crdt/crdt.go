package crdt

import "fmt"

type Nil struct{}

type Replica struct {
	// filename -> {userName -> rating}
	// DotMap<String, ORSet<(string, int)>>
	// DotMap<String, GSet<(string, int)>> visto que nao se pode mudar o rating
	Files         map[string]map[string]uint8
	Peers         map[string]Nil
	VersionVector map[uint32]uint64
}

func (replica Replica) AddFile(fileName string, currentID uint32) bool {

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

func (replica Replica) AddUser(userName string, currentID uint32) bool {

	_, ok := replica.Peers[userName]
	if !ok {
		replica.Peers[userName] = Nil{}
		replica.VersionVector[currentID]++
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

func (replica Replica) AddUserClassification(fileName string, userName string, classification uint8, currentID uint32) (fileExists bool, classificationAlreadyExists bool) {
	fileInfo, fileExists := replica.Files[fileName]

	if !fileExists {
		return
	}

	_, classificationAlreadyExists = fileInfo[userName]

	if !classificationAlreadyExists {
		fileInfo[userName] = classification
		replica.VersionVector[currentID]++
	}

	return
}

func (replica Replica) causalContextUnion(versionVector map[uint32]uint64) {

	for id := range versionVector {
		_, ok := replica.VersionVector[id]

		if !ok {
			replica.VersionVector[id] = versionVector[id]
		} else {
			replica.VersionVector[id] = max(replica.VersionVector[id], versionVector[id])
		}
	}
}

func (replica *Replica) peerJoin(peerReplica Replica) {
	newPeers := make(map[string]Nil)

	for peer := range replica.Peers {
		_, ok := peerReplica.Peers[peer]

		if !ok {
			newPeers[peer] = Nil{}
		}
	}

	for peer := range peerReplica.Peers {
		_, ok := replica.Peers[peer]

		if !ok {
			newPeers[peer] = Nil{}
		}
	}

	replica.Peers = newPeers
}

func (replica *Replica) fileJoin(peerReplica Replica) {

}

func (replica Replica) DSJoin(peerReplica Replica) {

	replica.peerJoin(peerReplica)

	replica.fileJoin(peerReplica)
}

func (replica Replica) Converge(peerReplica Replica) {

	replica.DSJoin(peerReplica)

	replica.causalContextUnion(peerReplica.VersionVector)
}
