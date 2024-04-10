-module(main_loop).
-export([mainLoop/1]).

handler({create, Username, Passwd}, {UserMap,_} = State, Dest) ->
    case maps:find(Username, UserMap) of
        {ok, _} ->
            Dest ! {user_exists, ?MODULE},
            Map;

        error ->
            Dest ! {ok, ?MODULE},
            maps:put(Username, {Passwd, offline}, Map)
    end;

handler({close, Username, Passwd}, Map, Dest) ->
    case maps:find(Username, Map) of
        {ok, {Passwd, _}} ->
            Dest ! {ok, ?MODULE},
            maps:remove(Username, Map);

        _ ->
            Dest ! {invalid, ?MODULE},
            Map
    end;

handler({login, Username, Passwd}, Map, Dest) ->
    case maps:find(Username, Map) of
        {ok, {Passwd, offline}} ->
            Dest ! {ok, ?MODULE},
            maps:update(Username, {Passwd, online}, Map);

        _ ->
            Dest ! {invalid, ?MODULE},
            Map
    end;
handler({logout, Username, Passwd}, Map, Dest) ->
    case maps:find(Username, Map) of
        {ok, {Passwd, offline}} ->
            Dest ! {ok, ?MODULE},
            maps:update(Username, {Passwd, online}, Map);

        _ ->
            Dest ! {invalid, ?MODULE},
            Map
    end.

mainLoop(State) ->
    receive
        {Msg, From} ->
            mainLoop(server_logic:handler(Msg, State, From))
    end.