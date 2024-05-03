package crdt

import "fmt"

type Nil struct{}

type VoteInfo struct {
	Sum   int
	Count int
}

func (voteInfo VoteInfo) joinInfos(peerVoteInfo VoteInfo) {
	voteInfo.Sum = max(voteInfo.Sum, peerVoteInfo.Sum)
	voteInfo.Count = max(voteInfo.Count, peerVoteInfo.Count)
}

func (voteInfo VoteInfo) incrementVote(classification int) {
	voteInfo.Sum += classification
	voteInfo.Count++
}

type Replica struct {
	// filename -> {userName -> rating}
	// DotMap<String, ORSet<(string, int)>>
	// DotMap<String, GSet<(string, int)>> visto que nao se pode mudar o rating
	// NEW idea: DotMap<String, GCounter<int,int>>
	Files         map[string]map[uint32]VoteInfo
	Peers         map[string]Nil
	VersionVector map[uint32]uint64
}

func (replica Replica) AddFile(fileName string, currentID uint32) bool {

	_, ok := replica.Files[fileName]
	if !ok {
		replica.Files[fileName] = make(map[uint32]VoteInfo)
		replica.VersionVector[currentID]++
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

func (replica Replica) AddUserClassification(fileName string, classification int, currentID uint32, voteTable map[string]bool) (fileExists bool, canVote bool) {
	voteMap, fileExists := replica.Files[fileName]
	canVote = voteTable[fileName]

	if !fileExists || !canVote {
		return
	}

	voteMap[currentID].incrementVote(classification)

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

	// s & s'
	for peer := range replica.Peers {
		_, ok := peerReplica.Peers[peer]

		if ok {
			newPeers[peer] = Nil{}
		}
	}

	// s | c'
	for peer := range replica.Peers {
		_, ok := peerReplica.Peers[peer]

		if !ok {
			newPeers[peer] = Nil{}
		}
	}

	// s' | c
	for peer := range peerReplica.Peers {
		_, ok := replica.Peers[peer]

		if !ok {
			newPeers[peer] = Nil{}
		}
	}

	replica.Peers = newPeers
}

func joinInfoMaps(infoMap map[uint32]VoteInfo, peerInfoMap map[uint32]VoteInfo) (newInfoMap map[uint32]VoteInfo) {

	newInfoMap = make(map[uint32]VoteInfo)

	for id := range infoMap {
		newInfoMap[id] = infoMap[id]

		info, ok := peerInfoMap[id]

		if ok {
			newInfoMap[id].joinInfos(info)
		}
	}

	for id := range peerInfoMap {

		_, exist := newInfoMap[id]

		if !exist {
			newInfoMap[id] = peerInfoMap[id]
		}
	}

	return
}

func (replica *Replica) fileJoin(peerReplica Replica) {
	newFiles := make(map[string]map[uint32]VoteInfo)

	for file := range peerReplica.Files {
		_, ok := replica.Files[file]

		if ok {
			newFiles[file] = joinInfoMaps(replica.Files[file], peerReplica.Files[file])
		} else {
			newFiles[file] = peerReplica.Files[file]
		}
	}

	for file := range replica.Files {
		_, ok := newFiles[file]

		if !ok {
			_, ok = peerReplica.Files[file]

			if ok {

				newFiles[file] = joinInfoMaps(replica.Files[file], peerReplica.Files[file])
			} else {
				newFiles[file] = replica.Files[file]
			}
		}
	}

	replica.Files = newFiles
}

func (replica Replica) DSJoin(peerReplica Replica) {

	replica.peerJoin(peerReplica)

	replica.fileJoin(peerReplica)
}

func (replica Replica) Converge(peerReplica Replica) {

	replica.DSJoin(peerReplica)

	replica.causalContextUnion(peerReplica.VersionVector)
}
