package crdt

import "fmt"

type Nil struct{}

type VersionVector map[uint32]uint64

type DotPair struct {
	Id      uint32
	Version uint64
}

type DotSet map[DotPair]bool

type VoteInfo struct {
	Sum   int
	Count int
}

type VoteMap map[uint32]VoteInfo

func joinInfos(voteInfo VoteInfo, peerVoteInfo VoteInfo) VoteInfo {
	voteInfo.Sum = max(voteInfo.Sum, peerVoteInfo.Sum)
	voteInfo.Count = max(voteInfo.Count, peerVoteInfo.Count)
	return voteInfo
}

func incrementVote(voteInfo VoteInfo, classification int) VoteInfo {
	voteInfo.Sum += classification
	voteInfo.Count++
	return voteInfo
}

type FileInfo struct {
	Votes  VoteMap
	DotSet DotSet
}

func (fileInfo FileInfo) joinInfos(versionVector VersionVector, peerVersionVector VersionVector, peerFileInfo FileInfo) FileInfo {
	return FileInfo{joinVoteMaps(fileInfo.Votes, peerFileInfo.Votes),
		joinDotSet(versionVector, peerVersionVector, fileInfo.DotSet, peerFileInfo.DotSet)}
}

type GroupInfo struct {
	DotSet DotSet
}

func (groupInfo GroupInfo) joinInfos(versionVector VersionVector, peerVersionVector VersionVector, peerGroupInfo GroupInfo) GroupInfo {
	return GroupInfo{joinDotSet(versionVector, peerVersionVector, groupInfo.DotSet, peerGroupInfo.DotSet)}
}

type Replica struct {
	//ORSet<File> + votação como valor, o que implica que o join vai ter em conta
	//ORSet<Username>
	Files         map[string]FileInfo
	GroupUsers    map[string]GroupInfo
	VersionVector VersionVector
}

func (replica *Replica) AddFile(fileName string, currentID uint32) bool {

	_, ok := replica.Files[fileName]
	if !ok {
		replica.VersionVector[currentID]++
		replica.Files[fileName] = FileInfo{make(map[uint32]VoteInfo), make(DotSet)}
		replica.Files[fileName].DotSet[DotPair{currentID, replica.VersionVector[currentID]}] = true
	}

	return !ok
}

func (replica *Replica) RemoveFile(fileName string) (ok bool) {

	_, ok = replica.Files[fileName]
	if ok {
		delete(replica.Files, fileName)
	}

	return
}

func (replica *Replica) ListFiles() {
	for fileName := range replica.Files {
		fmt.Println(fileName)
	}
}

func (replica *Replica) AddUser(userName string, currentID uint32) bool {

	_, ok := replica.GroupUsers[userName]
	if !ok {
		replica.VersionVector[currentID]++
		replica.GroupUsers[userName] = GroupInfo{make(DotSet)}
		replica.GroupUsers[userName].DotSet[DotPair{currentID, replica.VersionVector[currentID]}] = true
	}

	return !ok
}

func (replica *Replica) RemoveUser(userName string) (ok bool) {
	_, ok = replica.GroupUsers[userName]
	if ok {
		delete(replica.GroupUsers, userName)
	}

	return
}

func (replica *Replica) AddUserClassification(fileName string, classification int, currentID uint32, voteTable map[string]bool) (fileExists bool, canVote bool) {
	fileInfo, fileExists := replica.Files[fileName]
	canVote = voteTable[fileName]

	if !fileExists || !canVote {
		return
	}

	fileInfo.Votes[currentID] = incrementVote(fileInfo.Votes[currentID], classification)

	return
}

func (replica *Replica) causalContextUnion(versionVector map[uint32]uint64) {

	for id := range versionVector {
		_, ok := replica.VersionVector[id]

		if !ok {
			replica.VersionVector[id] = versionVector[id]
		} else {
			replica.VersionVector[id] = max(replica.VersionVector[id], versionVector[id])
		}
	}
}

func joinDotSet(causalContext VersionVector, peerCausalContext VersionVector,
	dotSet DotSet, peerDotSet DotSet) DotSet {
	newDotSet := make(DotSet)

	// s & s'
	for dotPair := range dotSet {

		if peerDotSet[dotPair] {
			newDotSet[dotPair] = true
		}
	}

	// s | c'
	for dotPair := range dotSet {
		version, ok := peerCausalContext[dotPair.Id]

		if !ok || version < dotPair.Version {
			newDotSet[dotPair] = true
		}
	}

	// s' | c
	for dotPair := range peerDotSet {
		version, ok := causalContext[dotPair.Id]

		if !ok || version < dotPair.Version {
			newDotSet[dotPair] = true
		}
	}

	return newDotSet
}

func joinVoteMaps(voteMap VoteMap, peerVoteMap VoteMap) VoteMap {
	newInfoMap := make(VoteMap)

	for user := range voteMap {
		info := voteMap[user]
		peerInfo, ok := peerVoteMap[user]

		if ok {
			newInfoMap[user] = joinInfos(info, peerInfo)
		} else {
			newInfoMap[user] = info
		}
	}

	for user := range peerVoteMap {

		_, exist := newInfoMap[user]

		if !exist {
			newInfoMap[user] = peerVoteMap[user]
		}
	}

	return newInfoMap
}

func (replica *Replica) joinGroupMaps(peerReplica Replica) {

	newInfoMap := make(map[string]GroupInfo)

	infoMap := replica.GroupUsers
	peerInfoMap := peerReplica.GroupUsers

	for user := range infoMap {
		info := infoMap[user]
		peerInfo, ok := peerInfoMap[user]

		if ok {
			newInfoMap[user] = info.joinInfos(replica.VersionVector, peerReplica.VersionVector, peerInfo)
		} else {
			newInfoMap[user] = info
		}
	}

	for user := range peerInfoMap {

		_, exist := newInfoMap[user]

		if !exist {
			newInfoMap[user] = peerInfoMap[user]
		}
	}

	replica.GroupUsers = newInfoMap
}

func (replica *Replica) joinFileMaps(peerReplica Replica) {

	newInfoMap := make(map[string]FileInfo)

	infoMap := replica.Files
	peerInfoMap := peerReplica.Files

	for hash := range infoMap {
		info := infoMap[hash]
		peerInfo, ok := peerInfoMap[hash]

		if ok {
			newInfoMap[hash] = info.joinInfos(replica.VersionVector, peerReplica.VersionVector, peerInfo)
		} else {
			newInfoMap[hash] = info
		}
	}

	for hash := range peerInfoMap {

		_, exist := newInfoMap[hash]

		if !exist {
			newInfoMap[hash] = peerInfoMap[hash]
		}
	}

	replica.Files = newInfoMap
}

func (replica *Replica) DSJoin(peerReplica Replica) {

	replica.joinFileMaps(peerReplica)

	replica.joinGroupMaps(peerReplica)
}

func (replica *Replica) Converge(peerReplica Replica) {

	replica.DSJoin(peerReplica)

	replica.causalContextUnion(peerReplica.VersionVector)
}
