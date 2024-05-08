-module(sessionManager).
-export([start/2, create_album/2]).

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

% {albumMetaData, map(userName)->{isInSession, voteTable}}
start(AlbumName, UserName) ->
    case file:read_file(AlbumName) of
        {ok, AlbumBin} ->
            {AlbumMetaData, UserMap} = erlang:binary_to_term(AlbumBin),
            case maps:find(UserName, UserMap) of
                {ok, _} ->
                    {ok, spawn(fun() -> loop({AlbumMetaData, UserMap}) end)};
                _ ->
                    get_album_no_permission
            end;
        _ ->
            get_album_error
    end.

loop(Album) ->
    ok.
