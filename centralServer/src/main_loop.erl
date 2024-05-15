-module(main_loop).
-export([mainLoop/2]).

handler({log_out, UserName}, {UserMap, Metadata, DataServers} = State) ->
    case maps:find(UserName, UserMap) of
        {ok, {online, Passwd}} ->
            {maps:update(UserName, {offline, Passwd}, UserMap), Metadata, DataServers};

        _ ->
            State
    end.

handler({register, {Username, Passwd}}, {UserMap, Metadata, DataServers} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, _} ->
            From ! {register_error, self()},
            State;

        error ->
            From ! {register_ok, self()},
            {maps:put(Username, {offline, Passwd}, UserMap), Metadata, DataServers}
    end;

handler({login, {Username, Passwd}}, {UserMap, Metadata, DataServers} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, {offline, Passwd}} ->
            From ! {login_ok, Username, DataServers, self()},
            {maps:update(Username, {online, Passwd}, UserMap), Metadata, DataServers};

        _ ->
            From ! {login_error, self()},
            State
    end;

handler({create_album, Username, AlbumName}, State, From) ->
    case sessionManager:create_album(AlbumName, Username) of
        Msg when Msg =:= file_exists; Msg =:= error ->
            From ! {create_album_error, self()};

        ok ->
            From ! {create_album_ok, self()}
    end,
    State;

handler({get_album, Username, AlbumName}, {Users, AlbumMap, DataServers} = State, From) ->
    case maps:find(AlbumName, AlbumMap) of
        {ok, Pid} ->
            Pid ! {join, Username, From, self()},
            State;

        _ ->
            case sessionManager:start(AlbumName, Username, self()) of
                {ok, Pid} ->
                    Pid ! {join, From, self()},
                    {Users, maps:put(AlbumName, Pid, AlbumMap), DataServers};

                ErrorMsg ->
                    From ! {ErrorMsg, self()},
                    State
            end
    end;

handler({end_session, AlbumName}, {Users, AlbumMap, DataServers}=State, From) ->
    case maps:find(AlbumName, AlbumMap) of
        {ok, From} ->
            NewAlbumMap = maps:remove(AlbumName, AlbumMap),
            {Users, NewAlbumMap, DataServers};

        _ ->
            io:format("Unkown end_session message"),
            State
    end;

handler({addServer, IP, PORT}, {Users, AlbumMap, {From, Servers}}=_State, From) ->
    %maps:foreach(fun(UserName, {Status, _}))
    {Users, AlbumMap, {From, [{IP, PORT} | Servers]}}.

mainLoop({Users, Albums}, Central) ->
    receive
        {DataLoop, Central} ->
            mainLoop({Users, Albums, {DataLoop, []}})
    end.

mainLoop(State) ->
    receive
        {log_out, UserName} ->
            mainLoop(handler({log_out, UserName}, State));

        {Msg, From} ->
            io:format("~p~n", [Msg]),
            mainLoop(handler(Msg, State, From))
    end.