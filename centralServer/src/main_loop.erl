-module(main_loop).
-export([mainLoop/1]).

handler({register, {Username, Passwd}}, {UserMap, OnlineMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, _} ->
            From ! {register_error, self()},
            State;

        error ->
            From ! {register_ok, self()},
            {maps:put(Username, Passwd, UserMap) ,OnlineMap, Metadata}
    end;

handler({login, {Username, Passwd}}, {UserMap, OnlineMap, Metadata} = State, From) ->
    case maps:find(Username, UserMap) of
        {ok, Passwd} ->
            From ! {login_ok, self()},
            {UserMap, maps:put(From, Username, OnlineMap), Metadata};

        _ ->
            From ! {login_error, self()},
            State
    end;

handler({log_out}, {UserMap, OnlineMap, Metadata} = State, From) ->
    case maps:find(From, OnlineMap) of
        {ok, _} ->
            {UserMap, maps:remove(From, OnlineMap), Metadata};

        _ ->
            State
    end;

handler({create_album, AlbumName}, {UserMap, OnlineMap, Metadata} = State, From) ->
    {ok, Username} = maps:find(From, OnlineMap),
    
    case maps:find(AlbumName, Metadata) of
        {ok, _} ->
            From ! {create_album_error, self()},
            State;
    
        _ ->
            From ! {create_album_ok, self()},
            {UserMap, OnlineMap, maps:put(AlbumName, {[Username], #{}}, Metadata)}
    end;

handler({get_album, AlbumName}, {_, OnlineMap, Metadata} = State, From) ->
    {ok, Username} = maps:find(From, OnlineMap),
    
    case maps:find(AlbumName, Metadata) of
        {ok, {Users, _}=AlbumData} ->
            case lists:member(Username, Users) of
                true ->
                    From ! {get_album_ok, AlbumData, self()};

                false ->
                    From ! {get_album_no_permission, self()}
            end,
            State;
    
        _ ->
            From ! {get_album_error, self()},
            State
    end.

mainLoop(State) ->
    receive
        {Msg, From} ->
            mainLoop(handler(Msg, State, From))
    end.