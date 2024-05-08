-module(sessionManager).
-export([start/3, create_album/2]).

% self-reminder, destruir CRDT no final da sessão e construir no inicio

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
                    {ok, spawn(fun() -> loop({AlbumMetaData, {[], 0}, UserMap, MainLoop}) end)};
                _ ->
                    get_album_no_permission
            end;
        _ ->
            get_album_error
    end.

prepare_replica_state(UserName, {IdPool, IdCounter}, UserMap) ->
    case IdPool of
        [] ->
            Id = IdCounter,
            NewIdInfo = {[], IdCounter + 1};
        [Id] ->
            NewIdInfo = {[], IdCounter};
        [Id | T] ->
            NewIdInfo = {[T], IdCounter}
    end,
    % falta ip,port
    SessionPeers = maps:filtermap(
        maps:filtermap(
            fun
                (_, {CurrId, _}) when CurrId =/= -1 ->
                    {ok, {true, CurrId}};
                (_, _) ->
                    false
            end,
            UserMap
        ),
        UserMap
    ),
    {_, Votetable} = maps:get(UserName, UserMap),
    {{Id, SessionPeers, Votetable}, NewIdInfo}.

handler(
    {join, Username, Client, MainLoop}, {AlbumMetaData, IdInfo, UserMap, MainLoop} = State, MainLoop
) ->
    case maps:find(Username, UserMap) of
        {ok, {-1, _}} ->
            {{Id, SessionPeers, Votetable}, NewIdInfo} = prepare_replica_state(
                Username, IdInfo, UserMap
            ),
            Client ! {get_album_ok, {Id, AlbumMetaData, SessionPeers, Votetable}, self()},
            {AlbumMetaData, NewIdInfo, UserMap, MainLoop};
        {ok, _} ->
            Client ! get_album_already_in_session,
            State;
        _ ->
            Client ! get_album_no_permission,
            State
    end;

handler({put_album, UserName, {Crdt, Votetable}}, {AlbumMetaData, IdInfo, UserMap, MainLoop}=State, Client) ->
    case maps:find(UserName, UserMap) of
        {ok, {Id, _}} ->
            

        {ok, {-1, _}} ->
            Client ! {put_album_not_in_session, self()},
            State;
        
        _ ->
            Client ! {put_album_no_permission, self()};
            State
    end.

loop(State) ->
    receive
        {Msg, From} ->
            loop(handler(Msg, State, From))
    end.
