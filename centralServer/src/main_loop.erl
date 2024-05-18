-module(main_loop).
-export([mainLoop/1]).

insert_server(Servers, Server, Position) ->
    insert_server(Servers, Server, Position, []).
insert_server([], Server, _Position, Acc) ->
    lists:reverse([Server | Acc]);
insert_server(Servers, Server, 0, Acc) ->
    lists:reverse([Server | Acc]) ++ Servers;
insert_server([H|T] = _Servers, Server, Position, Acc) ->
    insert_server(T, Server, Position-1, [H|Acc]).


handler({log_out, UserName}, {UserMap, Metadata, DataServers} = State) ->
    case maps:find(UserName, UserMap) of
        {ok, {From, Passwd}} when From =/= offline ->
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

handler({login, {Username, Passwd}}, {UserMap, Metadata, {_, Servers}=DataServers} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, {offline, Passwd}} ->
            From ! {login_ok, Username, Servers, self()},
            {maps:update(Username, {From, Passwd}, UserMap), Metadata, DataServers};

        _ ->
            From ! {login_error, self()},
            State
    end;

handler({create_album, Username, AlbumName}, State, From) ->
    case sessionManager:create_album(AlbumName, Username) of
        ok ->
            From ! {create_album_ok, self()};

        _ ->
            From ! {create_album_error, self()}

    end,
    State;

handler({get_album, Username, Ip, Port, AlbumName}, {Users, AlbumMap, DataServers} = State, From) ->
    case maps:find(AlbumName, AlbumMap) of
        {ok, Pid} ->
            Pid ! {{join, Username, Ip, Port, From}, self()},
            State;

        _ ->
            case sessionManager:start(AlbumName, Username, self()) of
                {ok, Pid} ->
                    Pid ! {{join, Username, Ip, Port, From}, self()},
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

handler({addServer, IP, PORT, Position}, {Users, AlbumMap, {From, Servers}}=_State, From) ->
    maps:foreach(fun(_UserName, {Pid, _Password}) ->
            Pid ! {{new_server, IP, PORT}, self()}
        end, Users),
    NewServers = insert_server(Servers, {IP, PORT}, Position),
    {Users, AlbumMap, {From, NewServers}}.

mainLoop({Users, Albums}) ->
    receive
        DataLoop ->
            mainLoop({Users, Albums, {DataLoop, []}})   
    end;

mainLoop(State) ->
    receive
        {log_out, UserName} ->
            mainLoop(handler({log_out, UserName}, State));

        {Msg, From} ->
            io:format("~p~n", [Msg]),
            mainLoop(handler(Msg, State, From))
    end.