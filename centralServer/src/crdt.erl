-module(crdt).
-export([createAlbum/1, updateMetaData/3, refreshUserMap/1]).

createAlbum(UserName) ->
    %Files         map[string]{Votes, DotSet}
    Files = #{},

    %GroupUsers    map[string]map[{Id, Version}]bool
    GroupUsers = maps:put(UserName, #{}, #{}),

    % VersionVector map[uint32]uint64
    VersionVector = #{},

    % Central Server logic - Votetable, map(userName)->{voteTable}
    UsersInfo = maps:put(UserName, #{}, #{}),

    {{Files, GroupUsers, VersionVector}, UsersInfo}.

refreshUserMap({{_, GroupUsers, _}=Metadata, UsersInfo}) ->
    NewUsersInfo = maps:map(fun(Name, _) ->
            case maps:find(Name, UsersInfo) of
                {ok, VoteTable} ->
                    VoteTable;

                _ ->
                    #{}
            end
        end, GroupUsers),
    {Metadata, NewUsersInfo}.


updateMetaData(
    {{Files, GroupUsers, VersionVector}, NewVotetable},
    {{OldFiles, OldGroupUsers, OldVersionVector}, UsersInfo},
    UserName
) ->
    UsersInfo = updateVoteTable(NewVotetable, UsersInfo, UserName),
    NewFiles = joinMaps(maps:to_list(OldFiles), maps:to_list(Files), {fun(Info, PeerInfo, FuncInfo) -> joinFileInfos(Info, PeerInfo, FuncInfo) end, {OldVersionVector, VersionVector}}),
    NewGroupUsers = joinMaps(maps:to_list(OldGroupUsers), maps:to_list(GroupUsers), {
        fun(
            Info, PeerInfo, FuncInfo
        ) ->
            joinGroupInfos(Info, PeerInfo, FuncInfo)
        end,
        {OldVersionVector, VersionVector}
    }),
    VV = causalContextUnion(OldVersionVector, maps:to_list(VersionVector)),
    {{NewFiles, NewGroupUsers, VV}, UsersInfo}.

updateVoteTable(NewVotetable, UsersInfo, UserName) ->
    case maps:find(UserName, UsersInfo) of
        {ok, _} ->
            maps:update(UserName, NewVotetable, UsersInfo);
        _ ->
            io:format("Error in updateVoteTable"),
            UsersInfo
    end.

% return DotSet in list format
joinDotSet(VV, PeerVV, DotSet, PeerDotSet) ->
    % S & S'
    NewDotSet = lists:filter(fun({DotPair, _}) -> lists:member(DotPair, PeerDotSet) end, DotSet),

    AddIf = fun(VersionVector) ->
        fun({{Id, Version}, _} = DotPairSet, Acc) ->
            case maps:find(Id, VersionVector) of
                {ok, Version2} ->
                    case Version2 < Version of
                        true ->
                            [DotPairSet | Acc];
                        _ ->
                            Acc
                    end;
                _ ->
                    [DotPairSet | Acc]
            end
        end
    end,

    % S | C'
    NewDotSet1 = lists:foldl(AddIf(PeerVV), NewDotSet, DotSet),

    % S' | C
    lists:foldl(AddIf(VV), NewDotSet1, PeerDotSet).

% Convert to Map because of golang
%maps:from_list(NewDotSet2).

joinVoteMaps(Votes, PeerVotes) ->
    JoinVoteInfos = fun({Sum, Count}, {PeerSum, PeerCount}, _) ->
        {max(Sum, PeerSum), max(Count, PeerCount)}
    end,

    joinMaps(Votes, PeerVotes, {JoinVoteInfos, ok}).

joinFileInfos({Votes, DotSet}, {PeerVotes, PeerDotSet}, {VV, PeerVV}) ->
    NewVotes = joinVoteMaps(Votes, PeerVotes),
    NewDotSet = joinDotSet(VV, PeerVV, DotSet, PeerDotSet),
    {NewVotes, NewDotSet}.

joinGroupInfos(Info, PeerInfo, {VV, PeerVV}) ->
    joinDotSet(VV, PeerVV, Info, PeerInfo).

joinFirstMap(Map, PeerMap, JoinFuncInfo) ->
    joinFirstMap(Map, PeerMap, #{}, JoinFuncInfo).
joinFirstMap([], _, NewerMap, _) ->
    NewerMap;
joinFirstMap([{Name, Value} | MapTail], PeerMap, NewMap, {JoinFunc, Info}) -> % todo, ver isto do peermap acho que assume que é map mas esta como lista
    case maps:find(Name, PeerMap) of
        {ok, PeerValue} ->
            NewerMap = JoinFunc(Value, PeerValue, Info);
        _ ->
            NewerMap = maps:put(Name, Value, NewMap)
    end,
    joinFirstMap(MapTail, PeerMap, NewerMap, JoinFunc).

joinSecondMap([], NewMap) ->
    NewMap;
joinSecondMap([{Name, Value} | PeerMapTail], NewMap) ->
    case maps:find(Name, NewMap) of
        error ->
            NewerMap = maps:put(Name, Value, NewMap);
        _ ->
            NewerMap = NewMap
    end,
    joinSecondMap(PeerMapTail, NewerMap).

joinMaps(Map, PeerMap, JoinFuncInfo) ->
    NewMap = joinFirstMap(Map, PeerMap, JoinFuncInfo),
    NewerMap = joinSecondMap(PeerMap, NewMap),
    NewerMap.

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
