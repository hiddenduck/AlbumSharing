-module(main_loop).
-export([mainLoop/1]).

handler({register, {Username, Passwd}}, {UserMap, OnlineMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, _} ->
            From ! {user_exists, self()},
            State;

        error ->
            From ! {ok, self()},
            {maps:put(Username, Passwd, UserMap) ,OnlineMap, Metadata}
    end;

handler({login, {Username, Passwd}}, {UserMap, OnlineMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, Passwd} ->
            From ! {ok, self()},
            {UserMap, maps:update(From, UserName, OnlineMap), Metadata}

        _ ->
            From ! {invalid, self()},
            State
    end;

handler({logout}, {UserMap, OnlineMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, Passwd} ->
            From ! {ok, self()},
            {UserMap, maps:remove(From, OnlineMap), Metadata}

        _ ->
            From ! {invalid, self()},
            State
    end.

handler(create_album, AlbumName, {UserMap, OnlineMap, Metadata} = State, From) ->
    case maps:find(AlbumName, OnlineMap) of
        {ok, Passwd} ->
            From ! {ok, self()},
            {UserMap, maps:remove(From, OnlineMap), Metadata}
    
        _ ->
            From ! {invalid, self()},
            State
    end.

mainLoop(State) ->
    receive
        {Msg, From} ->
            mainLoop(handler(Msg, State, From))
    end.