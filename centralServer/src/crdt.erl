-module(crdt).
-export([createAlbum/1, updateMetaData/3]).

createAlbum(UserName) ->
    %Files         map[string]FileInfo
    Files = #{},

    %GroupUsers    map[string]map[{Id, Version}]bool
    GroupUsers = maps:put(UserName, #{}, #{}),

    % VersionVector map[uint32]uint64
    VersionVector = #{},

    % Central Server logic - InSession + Votetable, map(userName)->{isInSession, voteTable}
    UsersInfo = #{},

    {{Files, GroupUsers, VersionVector}, UsersInfo}.

updateMetaData({{Files, GroupUsers, VersionVector}, NewVotetable}, {{OldFiles, OldGroupUsers, OldVersionVector}, UsersInfo}, UserName) ->
    UsersInfo = updateVoteTable(NewVotetable, UsersInfo, UserName),
    NewFiles = joinMaps(OldFiles, Files, fun(Info,PeerInfo) -> joinFileInfos(Info, PeerInfo) end),
    NewGroupUsers = joinMaps(maps:to_list(OldGroupUsers), maps:to_list(GroupUsers), fun(Info,PeerInfo) -> joinGroupInfos(Info, PeerInfo) end),
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

%% In progress
joinFileInfos(Info, PeerInfo) ->
    ok.

joinGroupInfos(Info, PeerInfo) ->
    ok.


joinFirstMap([], _, NewerMap, _) ->
    NewerMap;

joinFirstMap([ {Name, Value} | MapTail], PeerMap, NewMap, JoinFunc) ->
    case maps:find(Name, PeerMap) of
        {ok, PeerValue} ->
            NewerMap = JoinFunc(Value, PeerValue);

        _ ->
            NewerMap = maps:put(Name, Value, NewMap)
    end,
    joinFirstMap(MapTail, PeerMap, NewerMap, JoinFunc).

joinSecondMap([], NewMap) ->
    NewMap;

joinSecondMap([ {Name, Value} | PeerMapTail], NewMap) ->
    case maps:find(Name, NewMap) of
        error ->
            NewerMap = maps:put(Name, Value, NewMap);

        _ ->
            NewerMap = NewMap
    end,
    joinSecondMap(PeerMapTail, NewerMap).


joinMaps(Map, PeerMap, JoinFunc) ->
    NewMap = joinFirstMap(Map, PeerMap, #{}, JoinFunc),
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

