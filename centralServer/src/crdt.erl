-module(crdt).
-export([createAlbum/1, updateMetaData/2, refreshUserMap/1, clearState/1]).

createAlbum(UserName) ->
    %Files         map[string]{FileHash(string), VoteMap, DotSet} where VoteMap = map[uint32]VoteInfo and DotSet = map[DotPair]bool
    % and DotPair = {Id,Version}
    Files = #{},

    %GroupUsers    map[string]DotMap
    GroupUsers = maps:put(UserName, #{}, #{}),

    % VersionVector map[uint32]uint64
    VersionVector = #{},

    % Central Server logic - Votetable, map(userName)->{voteTable}
    UsersInfo = maps:put(UserName, #{}, #{}),

    {{Files, GroupUsers, VersionVector}, UsersInfo}.

refreshUserMap({{_, GroupUsers, _} = Metadata, UsersInfo}) ->
    NewUsersInfo = maps:map(
        fun(Name, _) ->
            case maps:find(Name, UsersInfo) of
                {ok, VoteTable} ->
                    VoteTable;
                _ ->
                    #{}
            end
        end,
        GroupUsers
    ),
    NewUsersInfo.

updateMetaData(
    {Files, GroupUsers, VersionVector},
    {OldFiles, OldGroupUsers, OldVersionVector}
) ->
    NewFiles = joinMaps(OldFiles, Files, {
        fun joinFileInfos/4, fun joinFileInfo/2, {OldVersionVector, VersionVector}
    }),
    NewGroupUsers = joinMaps(OldGroupUsers, GroupUsers, {
        fun joinGroupInfos/4, fun joinGroupInfo/2, {OldVersionVector, VersionVector}
    }),
    VV = causalContextUnion(OldVersionVector, maps:to_list(VersionVector)),
    {NewFiles, NewGroupUsers, VV}.

joinDotSet(VV, DotSet) ->
    NewDotSet = maps:filter(
        fun({ID, Version}) ->
            case maps:find(ID, VV) of
                {ok, PeerVersion} ->
                    case PeerVersion < Version of
                        true ->
                            true;
                        _ ->
                            false
                    end;
                _ ->
                    true
            end
        end,
        DotSet
    ),
    {NewDotSet, true}.

joinDotSets(VV, PeerVV, DotSet, PeerDotSet) ->
    MinFun = fun({Id, _}, _, MIN) ->
        case Id < MIN of
            true -> Id;
            _ -> MIN
        end
    end,
    M1 = maps:fold(MinFun, infinity, DotSet),
    M2 = maps:fold(MinFun, infinity, PeerDotSet),
    KeepCurrValues = M1 < M2,

    % S & S'
    NewDotSet = maps:filter(
        fun(DotPair, _) ->
            maps:is_key(DotPair, PeerDotSet)
        end,
        DotSet
    ),

    % Fun
    Fun = fun(VVInQuestion) ->
        fun({ID, Version} = DotPair, _, ACC) ->
            case maps:find(ID, VVInQuestion) of
                {ok, VVersion} ->
                    case VVersion < Version of
                        true ->
                            maps:put(DotPair, ACC);
                        _ ->
                            ACC
                    end;
                _ ->
                    maps:put(DotPair, ACC)
            end
        end
    end,

    % S | C'
    NewerDotSet = maps:fold(Fun(PeerVV), NewDotSet, DotSet),

    % S | C'
    EvenNewerDotSet = maps:fold(Fun(VV), NewerDotSet, PeerDotSet),

    {EvenNewerDotSet, KeepCurrValues}.

joinVoteMaps(Votes, PeerVotes) ->
    JoinVoteInfos = fun({Sum, Count}, {PeerSum, PeerCount}) ->
        {max(Sum, PeerSum), max(Count, PeerCount)}
    end,

    NewVoteMap = maps:map(
        fun(User, Info) ->
            case maps:find(User, PeerVotes) of
                {ok, PeerInfo} ->
                    JoinVoteInfos(Info, PeerInfo);
                _ ->
                    Info
            end
        end,
        Votes
    ),

    maps:fold(
        fun(User, Info, ACC) ->
            case maps:find(User, ACC) of
                {ok, _} ->
                    ACC;
                _ ->
                    maps:put(User, Info, ACC)
            end
        end,
        NewVoteMap,
        PeerVotes
    ).

joinFileInfos(VV, PeerVV, {Hash, Votes, DotSet}, {PeerHash, PeerVotes, PeerDotSet}) ->
    {NewDotSet, Ok} = joinDotSets(VV, PeerVV, DotSet, PeerDotSet),
    case length(maps:keys(NewDotSet)) of
        0 ->
            {nil, false};
        _ ->
            NewVoteMaps = joinVoteMaps(Votes, PeerVotes),
            case Ok of
                true -> FileHash = Hash;
                _ -> FileHash = PeerHash
            end,
            {{FileHash, NewVoteMaps, NewDotSet}, true}
    end.

joinFileInfo(VV, {Hash, Votes, DotSet}) ->
    {NewDotSet, _} = joinDotSet(VV, DotSet),
    case length(maps:keys(NewDotSet)) of
        0 ->
            {nil, false};
        _ ->
            {{Hash, Votes, NewDotSet}, true}
    end.

joinGroupInfos(VV, PeerVV, Info, PeerInfo) ->
    {DotSet, _} = joinDotSets(VV, PeerVV, Info, PeerInfo),
    case length(maps:keys(DotSet)) of
        0 ->
            {nil, false};
        _ ->
            {DotSet, true}
    end.

joinGroupInfo(VV, Info) ->
    {DotSet, _} = joinDotSet(VV, Info),
    case length(maps:keys(DotSet)) of
        0 ->
            {nil, false};
        _ ->
            {DotSet, true}
    end.

joinMaps(Map, PeerMap, {JoinInfos, JoinInfo, VV, PeerVV}) ->
    NewMap = maps:filtermap(
        fun(User, Info) ->
            case maps:find(User, PeerMap) of
                {ok, PeerInfo} ->
                    {NewInfo, Ok} = JoinInfos(VV, PeerVV, Info, PeerInfo);
                _ ->
                    {NewInfo, Ok} = JoinInfo(VV, Info)
            end,
            case Ok of
                true ->
                    {true, NewInfo};
                _ ->
                    false
            end
        end,
        Map
    ),

    maps:fold(
        fun(User, Info, ACC) ->
            case maps:find(User, ACC) of
                {ok, _} ->
                    ACC;
                _ ->
                    {NewInfo, Ok} = JoinInfo(PeerVV, Info),
                    case Ok of
                        true ->
                            maps:put(User, NewInfo, ACC);
                        _ ->
                            ACC
                    end
            end
        end,
        NewMap,
        PeerMap
    ).

causalContextUnion(VersionVector, []) ->
    VersionVector;
causalContextUnion(VersionVector, [{ID, NewVersion} | VVTail]) ->
    case maps:find(ID, VersionVector) of
        {ok, Version} ->
            NewID = max(Version, NewVersion),
            NewVV = maps:update(ID, NewID, VersionVector);
        _ ->
            NewVV = maps:put(ID, NewVersion, VersionVector)
    end,
    causalContextUnion(NewVV, VVTail).

clearState({Files, GroupUsers, VersionVector}) ->
    {todo, todo, todo}.
