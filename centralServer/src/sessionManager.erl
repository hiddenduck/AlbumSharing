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

% {albumMetaData, {IdPool, IdCounter}, #{String} -> Info}, map(userName)->{voteTable}} sessionID = -1 -> not in session, Info -> {IP, PORT, ID, PID}
start(AlbumName, UserName, MainLoop) ->
    case file:read_file(AlbumName) of
        {ok, AlbumBin} ->
            {AlbumMetaData, UserMap} = erlang:binary_to_term(AlbumBin),
            case maps:find(UserName, UserMap) of
                {ok, _} ->
                    {ok,
                        spawn(fun() -> loop({AlbumMetaData, {[], 0}, #{}, UserMap, MainLoop}) end)};
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
        fun
            (_, {IP, Port, _, Pid}) ->
                Pid ! {new_peer, {Ip, PORT, UserName}, self()},
                {IP, Port}
        end,
        SessionUsers
    ),
    {{Id, SessionPeers}, NewIdInfo}.

handler(
    {join, Username, Ip, Port, Client, MainLoop},
    {AlbumMetaData, IdInfo, SessionUsers, UserMap, MainLoop} = State,
    MainLoop
) ->
    case maps:find(Username, SessionUsers) of
        {ok, _} ->
            Client ! get_album_already_in_session,
            State;
        _ ->
            case maps:find(Username, UserMap) of
                {ok, VoteTable} ->
                    {{Id, SessionPeers}, NewIdInfo} = prepare_replica_state(
                        Username, IdInfo, SessionUsers, Ip, Port
                    ),
                    Client ! {get_album_ok, {Id, AlbumMetaData, SessionPeers, VoteTable}, self()},
                    {AlbumMetaData, NewIdInfo, maps:put(UserName, {Ip, Port, Id, Client}, SessionUsers), UserMap, MainLoop};

                _ ->
                    Client ! get_album_no_permission,
                    State
            end
    end;

handler({put_album, UserName, {Crdt, Votetable}}, {AlbumMetaData, IdInfo, SessionUsers, UserMap, MainLoop}=State, Client) ->
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
