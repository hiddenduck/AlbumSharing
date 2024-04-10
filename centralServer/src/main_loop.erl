-module(main_loop).
-export([mainLoop/1]).

handler({create, Username, Passwd}, {UserMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, _} ->
            From ! {user_exists, self()},
            State;

        error ->
            From ! {ok, self()},
            {maps:put(Username, Passwd, UserMap), Metadata}
    end;

handler({close, Username, Passwd}, {UserMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, Passwd} ->
            From ! {ok, self()},
            {maps:remove(Username, UserMap), Metadata};

        _ ->
            From ! {invalid, self()},
            State
    end;

handler({login, Username, Passwd}, {UserMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, Passwd} ->
            From ! {ok, self()},
            maps:update(Username, {Passwd, online}, UserMap);

        _ ->
            From ! {invalid, self()},
            Map
    end;
handler({logout, Username, Passwd}, Map, From) ->
    case maps:find(Username, Map) of
        {ok, Passwd} ->
            From ! {ok, self()},
            maps:update(Username, {Passwd, online}, Map);

        _ ->
            From ! {invalid, self()},
            Map
    end.

mainLoop(State) ->
    receive
        {Msg, From} ->
            mainLoop(handler(Msg, State, From))
    end.