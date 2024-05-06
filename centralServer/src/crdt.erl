-module(crdt).
-export([createAlbum/1, updateMetaData/2]).

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

updateMetaData({NewAlbumMetaData, Votetable}, Metadata) ->
    ok.