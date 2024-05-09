package crdt

import (
	"fmt"
)

type Nil struct{}

type VersionVector map[uint32]uint64

type DotPair struct {
	Id      uint32
	Version uint64
}

type DotSet map[DotPair]bool

func listDots(dotSet DotSet) {

	for dotPair := range dotSet {
		fmt.Printf("\t%+v,", dotPair)
	}
	fmt.Printf("\n")
}

type VoteInfo struct {
	Sum   uint64
	Count uint64
}

type VoteMap map[uint32]VoteInfo

func averageVotes(voteMap VoteMap) float64 {
	var sum uint64
	var count uint64
	for id := range voteMap {
		sum += voteMap[id].Sum
		count += voteMap[id].Count
	}
	return float64(sum) / float64(count)
}

type FileInfo struct {
	FileHash string
	Votes    VoteMap
	DotSet   DotSet
}

type GroupInfo struct {
	DotSet DotSet
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
	fmt.Println("Files:")
	for fileName := range replica.Files {
		fmt.Printf("%v, Average Votes: %v\n", fileName, averageVotes(replica.Files[fileName].Votes))
	}
}

func (replica *Replica) ListUsers() {
	fmt.Println("Group Users:")
	for userName := range replica.GroupUsers {
		fmt.Printf("%v ", userName)
	}
	fmt.Printf("\n")
}

func (replica *Replica) ListVV() {
	fmt.Printf("Version Vector: ")
	fmt.Printf("%+v\n", replica.VersionVector)
}

func (replica *Replica) ListReplica() {
	fmt.Printf("%+v\n", *replica)
}
