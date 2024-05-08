-module(sessionManager).
-export([start/3, create_album/2]).

create_album(AlbumName, UserName) ->
    case file:read_file_info(AlbumName) of
        {ok, _} ->
            file_exists;
        {error, _} ->
            error;
        _ ->
            Album = crdt:createAlbum(UserName),
            AlbumBin = term_to_binary(Album),
            case file:write_file(AlbumName, AlbumBin) of
                ok ->
                    ok;
                {error, _} ->
                    error
            end
    end.

% {albumMetaData, [IdPool], map(userName)->{sessionID, voteTable}} sessionID = -1 -> not in session
start(AlbumName, UserName, MainLoop) ->
    case file:read_file(AlbumName) of
        {ok, AlbumBin} ->
            {AlbumMetaData, UserMap} = erlang:binary_to_term(AlbumBin),
            case maps:find(UserName, UserMap) of
                {ok, _} ->
                    {ok, spawn(fun() -> loop({AlbumMetaData, {[],0}, UserMap, MainLoop}) end)};
                _ ->
                    get_album_no_permission
            end;
        _ ->
            get_album_error
    end.

prepare_replica_state(UserName, AlbumMetaData, {IdPool, IdCounter}, UserMap) ->
    case IdPool of
        [] ->
            Id = IdCounter,
            NewIdInfo = {[], IdCounter+1};

        [Id] ->
            NewIdInfo = {[], IdCounter};

        [Id | T] ->
            NewIdInfo = {[T], IdCounter}
    end,
    
    {{Id, }, NewIdInfo}.

handler({join, Username, Client, MainLoop}, {AlbumMetaData, IdInfo, UserMap, MainLoop}, MainLoop) ->
    case maps:find(Username, UserMap) of
        {ok, {-1, _}} ->
            {{Id, Crdt, SessionPeers, Votetable}, NewIdInfo} = prepare_replica_state(Username, AlbumMetaData, IdInfo, UserMap),
            Client ! {get_album_ok, {Id, Crdt, SessionPeers, Votetable}, self()};

        {ok, _} ->
            Client ! get_album_already_in_session;

        _ ->
            Client ! get_album_no_permission
    end.

loop(State) ->
    receive
        {Msg, From} ->
            loop(handler(Msg, State, From))
    end.
