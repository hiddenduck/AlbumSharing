package crdt

func joinInfos(voteInfo VoteInfo, peerVoteInfo VoteInfo) VoteInfo {
	voteInfo.Sum = max(voteInfo.Sum, peerVoteInfo.Sum)
	voteInfo.Count = max(voteInfo.Count, peerVoteInfo.Count)
	return voteInfo
}

func incrementVote(voteInfo VoteInfo, classification uint64) VoteInfo {
	voteInfo.Sum += classification
	voteInfo.Count++
	return voteInfo
}

func (fileInfo FileInfo) joinInfo(peerVersionVector VersionVector) (FileInfo, bool) {
	DotSet := joinDotSet(nil, peerVersionVector, fileInfo.DotSet, nil)
	if len(DotSet) == 0 {
		return FileInfo{}, false
	} else {
		return FileInfo{fileInfo.FileHash, fileInfo.Votes, DotSet}, true
	}
}

func (fileInfo FileInfo) joinInfos(versionVector VersionVector, peerVersionVector VersionVector, peerFileInfo FileInfo) (FileInfo, bool) {
	DotSet := joinDotSet(versionVector, peerVersionVector, fileInfo.DotSet, peerFileInfo.DotSet)
	if len(DotSet) == 0 {
		return FileInfo{}, false
	} else {
		return FileInfo{fileInfo.FileHash, joinVoteMaps(fileInfo.Votes, peerFileInfo.Votes), DotSet}, true
	}
}

func (groupInfo GroupInfo) joinInfo(peerVersionVector VersionVector) (GroupInfo, bool) {
	DotSet := joinDotSet(nil, peerVersionVector, groupInfo.DotSet, nil)
	if len(DotSet) == 0 {
		return GroupInfo{}, false
	} else {
		return GroupInfo{DotSet}, true
	}
}

func (groupInfo GroupInfo) joinInfos(versionVector VersionVector, peerVersionVector VersionVector, peerGroupInfo GroupInfo) (GroupInfo, bool) {
	DotSet := joinDotSet(versionVector, peerVersionVector, groupInfo.DotSet, peerGroupInfo.DotSet)
	if len(DotSet) == 0 {
		return GroupInfo{}, false
	} else {
		return GroupInfo{DotSet}, true
	}
}

func (replica *Replica) AddFile(fileName string, fileHash string) bool {

	_, ok := replica.Files[fileName]
	if !ok {
		replica.VersionVector[replica.CurrentID]++
		replica.Files[fileName] = FileInfo{fileHash, make(map[uint32]VoteInfo), make(DotSet)}
		replica.Files[fileName].DotSet[DotPair{replica.CurrentID, replica.VersionVector[replica.CurrentID]}] = true
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

func (replica *Replica) AddUser(userName string) bool {

	_, ok := replica.GroupUsers[userName]
	if !ok {
		replica.VersionVector[replica.CurrentID]++
		replica.GroupUsers[userName] = GroupInfo{make(DotSet)}
		replica.GroupUsers[userName].DotSet[DotPair{replica.CurrentID, replica.VersionVector[replica.CurrentID]}] = true
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

func (replica *Replica) AddUserClassification(fileName string, classification uint64, voteTable *map[string]bool) (fileExists bool, cantVote bool) {
	fileInfo, fileExists := replica.Files[fileName]
	cantVote = (*voteTable)[fileName]

	if !fileExists || cantVote {
		return
	}

	fileInfo.Votes[replica.CurrentID] = incrementVote(fileInfo.Votes[replica.CurrentID], classification)
	(*voteTable)[fileName] = true

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
	if peerDotSet != nil {
		for dotPair := range dotSet {

			if peerDotSet[dotPair] {
				newDotSet[dotPair] = true
			}
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
	if peerDotSet != nil {
		for dotPair := range peerDotSet {
			version, ok := causalContext[dotPair.Id]

			if !ok || version < dotPair.Version {
				newDotSet[dotPair] = true
			}
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
	var newInfo GroupInfo

	for user := range infoMap {
		info := infoMap[user]
		peerInfo, ok := peerInfoMap[user]

		if ok {
			newInfo, ok = info.joinInfos(replica.VersionVector, peerReplica.VersionVector, peerInfo)
		} else {
			newInfo, ok = info.joinInfo(peerReplica.VersionVector)
		}

		if ok {
			newInfoMap[user] = newInfo
		}
	}

	for user := range peerInfoMap {

		_, exist := newInfoMap[user]

		if !exist {
			newInfo, ok := peerInfoMap[user].joinInfo(replica.VersionVector)
			if ok {
				newInfoMap[user] = newInfo
			}
		}
	}

	replica.GroupUsers = newInfoMap
}

func (replica *Replica) joinFileMaps(peerReplica Replica) {

	newInfoMap := make(map[string]FileInfo)

	infoMap := replica.Files
	peerInfoMap := peerReplica.Files
	var newInfo FileInfo

	for user := range infoMap {
		info := infoMap[user]
		peerInfo, ok := peerInfoMap[user]

		if ok {
			newInfo, ok = info.joinInfos(replica.VersionVector, peerReplica.VersionVector, peerInfo)
		} else {
			newInfo, ok = info.joinInfo(peerReplica.VersionVector)
		}

		if ok {
			newInfoMap[user] = newInfo
		}
	}

	for user := range peerInfoMap {

		_, exist := newInfoMap[user]

		if !exist {
			newInfo, ok := peerInfoMap[user].joinInfo(replica.VersionVector)
			if ok {
				newInfoMap[user] = newInfo
			}
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
