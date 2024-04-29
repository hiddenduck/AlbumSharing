package crdt


type crdt struct {
    // filename -> {userName -> rating}
    // DotMap<String, ORSet<(string, int)>>
    // DotMap<String, GSet<(string, int)>> visto que nao se pode mudar o rating
    VersionVector map[uint32]uint64
}
