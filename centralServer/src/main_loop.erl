-module(main_loop).
-export([mainLoop/1]).

handler({log_out, UserName}, {UserMap, Metadata} = State) ->
    case maps:find(UserName, UserMap) of
        {ok, {online, Passwd}} ->
            {maps:update(UserName, {offline, Passwd}, UserMap), Metadata};

        _ ->
            State
    end.

handler({register, {Username, Passwd}}, {UserMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, _} ->
            From ! {register_error, self()},
            State;

        error ->
            From ! {register_ok, self()},
            {maps:put(Username, {offline, Passwd}, UserMap), Metadata}
    end;

handler({login, {Username, Passwd}}, {UserMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, {offline, Passwd}} ->
            From ! {login_ok, Username, self()},
            {maps:update(Username, {online, Passwd}, UserMap), Metadata};

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

handler({get_album, Username, AlbumName}, {Users, AlbumMap} = State, From) ->
    case maps:find(AlbumName, AlbumMap) of
        {ok, Pid} ->
            Pid ! {join, Username, From, self()},
            State;

        _ ->
            case sessionManager:start(AlbumName, Username, self()) of
                {ok, Pid} ->
                    Pid ! {join, From, self()},
                    {Users, maps:put(AlbumName, Pid, AlbumMap)};

                ErrorMsg ->
                    From ! {ErrorMsg, self()},
                    State
            end
    end.

mainLoop(State) ->
    receive
        {log_out, UserName} ->
            mainLoop(handler({log_out, UserName}, State));

        {Msg, From} ->
            io:format("~p~n", [Msg]),
            mainLoop(handler(Msg, State, From))
    end.