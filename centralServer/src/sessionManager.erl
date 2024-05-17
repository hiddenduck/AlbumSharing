-module(sessionManager).
-export([start/3, create_album/2]).

% self-reminder, destruir CRDT no final da sessão e construir no inicio

create_album(AlbumName, UserName) ->
    case file:read_file_info(AlbumName) of
        {ok, _} ->
            file_exists;
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

saveState(AlbumName, AlbumMetaData, UserMap) ->
    ClearedAlbumMetaData = crdt:clearState(AlbumMetaData),
    NewUserMap = crdt:refreshUserMap({AlbumMetaData, UserMap}),
    AlbumBin = term_to_binary({ClearedAlbumMetaData, NewUserMap}),
    case file:write_file(AlbumName, AlbumBin) of
                ok ->
                    io:format("salvo~p~n", [AlbumMetaData]),
                    ok;
                {error, _} ->
                    error
    end.
    

% {albumMetaData, {IdPool, IdCounter}, #{String} -> Info}, map(userName)->{voteTable}} sessionID = -1 -> not in session, Info -> {IP, PORT, ID, PID}
start(AlbumName, UserName, MainLoop) ->
    case file:read_file(AlbumName) of
        {ok, AlbumBin} ->
            {AlbumMetaData, UserMap} = erlang:binary_to_term(AlbumBin),
            case maps:find(UserName, UserMap) of
                {ok, _} ->
                    {ok,
                        spawn(fun() -> loop(AlbumName, {AlbumMetaData, {[], 0}, #{}, UserMap, MainLoop}) end)};
                _ ->
                    get_album_no_permission
            end;
        _ ->
            get_album_error
    end.

prepare_replica_state(UserName, {IdPool, IdCounter}, SessionUsers, Ip, PORT) ->
    case IdPool of
        [] ->
            Id = IdCounter,
            NewIdInfo = {[], IdCounter + 1};
        [Id] ->
            NewIdInfo = {[], IdCounter};
        [Id | T] ->
            NewIdInfo = {[T], IdCounter}
    end,
    SessionPeers = maps:map(
        fun(_, {IP, Port, PeerId, Pid}) ->
            Pid ! {new_peer, {Ip, PORT, UserName, PeerId}, self()},
            {IP, Port, integer_to_list(PeerId)}
        end,
        SessionUsers
    ),
    {{Id, SessionPeers}, NewIdInfo}.

handler(
    {join, Username, Ip, Port, Client},
    {AlbumMetaData, IdInfo, SessionUsers, UserMap, MainLoop} = State,
    MainLoop, AlbumName
) ->
    case maps:find(Username, SessionUsers) of
        {ok, _} ->
            Client ! get_album_already_in_session,
            loop(AlbumName, State);
        _ ->
            case maps:find(Username, UserMap) of
                {ok, VoteTable} ->
                    {{Id, SessionPeers}, NewIdInfo} = prepare_replica_state(
                        Username, IdInfo, SessionUsers, Ip, Port
                    ),
                    Client ! {get_album_ok, {Id, AlbumMetaData, SessionPeers, VoteTable}, self()},
                    loop(AlbumName,{AlbumMetaData, NewIdInfo,
                        maps:put(Username, {Ip, Port, Id, Client}, SessionUsers), UserMap,
                        MainLoop});
                _ ->
                    Client ! get_album_no_permission,
                    loop(AlbumName,State)
            end
    end;
handler(
    {put_album, UserName, {Crdt, Votetable}},
    {AlbumMetaData, {IdPool, IdCounter}, SessionUsers, UserMap, MainLoop} = State,
    Client, AlbumName
) ->
    case maps:find(UserName, UserMap) of
        {ok, _} ->
            io:format("~p~n", [Crdt]),
            NewAlbumMetaData = crdt:updateMetaData(Crdt, AlbumMetaData),
            NewUserMap = maps:update(UserName, Votetable, UserMap),
            {_, _, Id, _} = maps:get(UserName, SessionUsers),
            NewSessionUsers = maps:remove(UserName, SessionUsers),
            Client ! {put_album_ok, MainLoop, self()},
            case NewSessionUsers of
                #{} ->
                    MainLoop ! {{end_session, AlbumName}, self()},
                    saveState(AlbumName, NewAlbumMetaData, NewUserMap),
                    end_loop();

                _ ->
                    maps:foreach(fun(_, {_, _, _, PID}) ->
                            PID ! {peer_left, UserName, self()}
                        end,
                        NewSessionUsers),
                    loop(AlbumName,{NewAlbumMetaData, {[Id | IdPool], IdCounter}, NewSessionUsers, NewUserMap,
                        MainLoop})
            end;
        _ ->
            Client ! {put_album_no_permission, self()},
            loop(AlbumName, State)
    end.

end_loop() ->
    receive
        {_, From} ->
            From ! get_album_error 
        after 0 ->
            ok
    end.

loop(AlbumName, State) ->
    receive
        {Msg, From} ->
            io:format("Session: ~p~n", [Msg]),
            handler(Msg, State, From, AlbumName)
    end.
