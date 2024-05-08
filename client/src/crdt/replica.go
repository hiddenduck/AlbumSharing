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
	Sum   uint64
	Count uint64
}

type VoteMap map[uint32]VoteInfo

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
	CurrentID     uint32
}

func CreateReplica(id uint32) Replica {
	return Replica{make(map[string]FileInfo), make(map[string]GroupInfo), make(VersionVector), id}
}

func (replica *Replica) ListFiles() {
	for fileName := range replica.Files {
		fmt.Println(fileName)
	}
}

func (replica *Replica) ReplicaToString() {

}
