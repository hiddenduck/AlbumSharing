-module(main_loop).
-export([mainLoop/1]).

% Album Metadata
createAlbumMetaData() -> % replica data
    {
    %Files         map[string]FileInfo
	#{},
    %GroupUsers    map[string]GroupInfo
    #{},
    % VersionVector map[uint32]uint64
    #{}
    }.

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
            {Files, Users, VV} = createAlbumMetaData(),
            {UserMap, OnlineMap, maps:put(AlbumName, {{Files, [Username | Users], VV}, #{}}, Metadata)}
    end;

handler({get_album, AlbumName}, {_, OnlineMap, Metadata} = State, From) ->
    {ok, Username} = maps:find(From, OnlineMap),
    
    case maps:find(AlbumName, Metadata) of
        {ok, {AlbumMetaData, UserMap}} ->
            case maps:find(Username, UserMap) of
                {ok, {true, Votetable}} ->
                    From ! {get_album_ok, {AlbumMetaData, Votetable}, self()};

                _ ->
                    From ! {get_album_no_permission, self()}
            end,
            State;
    
        _ ->
            From ! {get_album_error, self()},
            State
    end;

handler({put_album, AlbumName}, {_, OnlineMap, Metadata} = State, From) ->
    {ok, Username} = maps:find(From, OnlineMap),

    case maps:find(AlbumName, Metadata) of
        {ok, {_, UserMap}=AlbumData} ->
            case maps:find(Username, UserMap) of
                true ->
                    From ! {put_album_ok, AlbumData, self()};

                _ ->
                    From ! {put_album_no_permission, self()}
            end,
            State;
    
        _ ->
            From ! {get_album_error, self()},
            State
    end.

mainLoop(State) ->
    receive
        {Msg, From} ->
            io:format("~p~n", [Msg]),
            mainLoop(handler(Msg, State, From))
    end.