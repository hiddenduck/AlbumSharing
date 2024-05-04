package crdt

import "fmt"

type Nil struct{}

type DotPair struct {
	Id      uint32
	Version uint64
}

type DotSet map[DotPair]bool

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

type FileInfo struct {
	Votes  map[uint32]VoteInfo
	DotSet DotSet
}

type GroupInfo struct {
	DotSet DotSet
}

type Replica struct {
	// filename -> {userName -> rating}
	// DotMap<String, ORSet<(string, int)>>
	// DotMap<String, GSet<(string, int)>> visto que nao se pode mudar o rating
	// NEW idea: DotMap<String, GCounter<int,int>>
	//ORSet<File> + votação como valor, o que implica que o join vai ter em conta
	Files map[string]FileInfo
	//ORSet<Username>
	GroupUsers    map[string]GroupInfo
	VersionVector map[uint32]uint64
}

func (replica Replica) AddFile(fileName string, currentID uint32) bool {

	_, ok := replica.Files[fileName]
	if !ok {
		replica.VersionVector[currentID]++
		replica.Files[fileName] = FileInfo{make(map[uint32]VoteInfo), make(DotSet)}
		replica.Files[fileName].DotSet[DotPair{currentID, replica.VersionVector[currentID]}] = true
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

	_, ok := replica.GroupUsers[userName]
	if !ok {
		replica.VersionVector[currentID]++
		replica.GroupUsers[userName] = GroupInfo{make(DotSet)}
		replica.GroupUsers[userName].DotSet[DotPair{currentID, replica.VersionVector[currentID]}] = true
	}

	return !ok
}

func (replica Replica) RemoveUser(userName string) (ok bool) {
	_, ok = replica.GroupUsers[userName]
	if ok {
		delete(replica.GroupUsers, userName)
	}

	return
}

func (replica Replica) AddUserClassification(fileName string, classification int, currentID uint32, voteTable map[string]bool) (fileExists bool, canVote bool) {
	voteInfo, fileExists := replica.Files[fileName]
	canVote = voteTable[fileName]

	if !fileExists || !canVote {
		return
	}

	voteInfo.Votes[currentID].incrementVote(classification)

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
	for peer := range replica.GroupUsers {
		_, ok := peerReplica.GroupUsers[peer]

		if ok {
			newPeers[peer] = Nil{}
		}
	}

	// s | c'
	for peer := range replica.GroupUsers {
		_, ok := peerReplica.VersionVector[peer]

		if !ok {
			newPeers[peer] = Nil{}
		}
	}

	// s' | c
	for peer := range peerReplica.GroupUsers {
		_, ok := replica.VersionVector[peer]

		if !ok {
			newPeers[peer] = Nil{}
		}
	}

	replica.GroupUsers = newPeers
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
