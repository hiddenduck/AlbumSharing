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

handler({create_album, Username, AlbumName}, {UserMap, Metadata} = State, From) ->
    case maps:find(AlbumName, Metadata) of
        {ok, _} ->
            From ! {create_album_error, self()},
            State;
    
        _ ->
            From ! {create_album_ok, self()},
            {UserMap, maps:put(AlbumName, crdt:createAlbum(Username), Metadata)}
    end;

handler({get_album, Username, AlbumName}, {Users, Metadata} = State, From) -> % todo: trocar para fazer match
    case maps:find(AlbumName, Metadata) of
        {ok, {AlbumMetaData, UserMap}} ->
            case maps:find(Username, UserMap) of
                {ok, {false, Votetable}} ->
                    From ! {get_album_ok, {AlbumMetaData, Votetable}, self()},
                    NewUserMap = maps:update(Username, {true, Votetable}, UserMap),
                    {Users, maps:update(AlbumName, {AlbumMetaData, NewUserMap}, Metadata)};

                {ok, {true, _}} ->
                    From ! {get_album_already_in_session, self()},
                    State;

                _ ->
                    From ! {get_album_no_permission, self()},
                    State
            end;
            
    
        _ ->
            From ! {get_album_error, self()},
            State
    end;

handler({put_album, UserName, AlbumName, Changes}, {Users, Metadata} = State, From) ->
    case maps:find(AlbumName, Metadata) of
        {ok, {_, UserMap}=AlbumInfo} ->
            case maps:find(UserName, UserMap) of
                {ok, {true, _}} ->
                    NewAlbumInfo = crdt:updateMetaData(Changes, AlbumInfo, UserName),
                    From ! {put_album_ok, self()},
                    {Users, maps:update(AlbumName, NewAlbumInfo, Metadata)};

                {ok, {false, _}} ->
                    From ! {put_album_not_in_session, self()},
                    State;

                _ ->
                    From ! {put_album_no_permission, self()},
                    State
            end;
    
        _ ->
            From ! {get_album_error, self()},
            State
    end.

mainLoop(State) ->
    receive
        {log_out, UserName} ->
            mainLoop(handler({log_out, UserName}, State));

        {Msg, From} ->
            io:format("~p~n", [Msg]),
            mainLoop(handler(Msg, State, From))
    end.