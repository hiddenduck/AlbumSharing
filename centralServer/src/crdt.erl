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

updateMetaData({{Files, GroupUsers, VersionVector}=NewAlbumMetaData, NewVotetable}, {{OldFiles, OldGroupUsers, OldVersionVector}=AlbumMetaData, UsersInfo}, UserName) ->
    UsersInfo = updateVoteTable(NewVotetable, UsersInfo, UserName),
    NewFiles = joinFileMaps(),
    NewGroupUsers = joinMaps(maps:to_list(OldGroupUsers), maps:to_list(GroupUsers)),
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

joinFileMaps() ->
    ok.

%% In progress
joinFirstMap() ->
    ok.

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


joinMaps(Map, PeerMap) ->
    NewMap = joinFirstMap(),
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

