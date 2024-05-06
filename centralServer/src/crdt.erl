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

updateMetaData({{Files, GroupUsers, VersionVector}=NewAlbumMetaData, NewVotetable}, {AlbumMetaData, UsersInfo}, UserName) ->
    ok.

updateVoteTable(NewVotetable, UsersInfo, UserName) ->
    case maps:find(UserName, UsersInfo) of
        {ok, {_, Votetable}} ->
            atualizar;

        _ ->
            io:format("Error in updateVoteTable")
    end.
