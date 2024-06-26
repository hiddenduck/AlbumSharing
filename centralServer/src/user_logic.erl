-module(user_logic).
-export([user/2]).
-define(ACTIVE_TIMES, 10).
-include("proto_generated/message.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% In Session %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
session_user_handler({{new_server, IP, PORT}, _MainLoop}, Sock, SessionLoop, UserName) ->
    Data = message:encode_msg(#'Message'{
        type = 2,
        msg =
            {m8, #newServer{
                ip = IP,
                port = integer_to_list(PORT)
            }}
    }),
    send(Data, Sock),
    session_user(Sock, SessionLoop, UserName);
session_user_handler({Status, SessionLoop}, Sock, SessionLoop, UserName) when
    Status =:= put_album_no_permission
->
    send_reply(atom_to_list(Status), Sock),
    auth_user(Sock, SessionLoop, UserName);
session_user_handler(
    {new_peer, {Ip, PORT, PeerUserName, PeerId}, SessionLoop}, Sock, SessionLoop, UserName
) ->
    io:format("PeerId to send: ~p~n", [PeerId]),
    Data = message:encode_msg(#'Message'{
        type = 8,
        msg =
            {m7, #peer{
                name = PeerUserName,
                peerInfo = #peerInfo{
                    ip = Ip,
                    port = PORT,
                    id = PeerId
                }
            }}
    }),
    send(Data, Sock),
    session_user(Sock, SessionLoop, UserName);
session_user_handler(
    {peer_left, {Ip, PORT, PeerUserName, PeerId}, SessionLoop}, Sock, SessionLoop, UserName
) ->
    Data = message:encode_msg(#'Message'{
        type = 9,
        msg =
            {m7, #peer{
                name = PeerUserName,
                peerInfo = #peerInfo{
                    ip = Ip,
                    port = PORT,
                    id = PeerId
                }
            }}
    }),
    send(Data, Sock),
    session_user(Sock, SessionLoop, UserName);
session_user_handler({put_album_ok, MainLoop, SessionLoop}, Sock, SessionLoop, UserName) ->
    send_reply("put_album_ok", Sock),
    auth_user(Sock, MainLoop, UserName).

session_message_handler(
    quit,
    {m4, #quitMessage{
        crdt = #crdt{
            versionVector = VV,
            files = Files,
            groupUsers = GroupUsers
        },
        voteTable = Votetable
    }},
    Sock,
    SessionLoop,
    UserName
) ->
    ParsedFiles = maps:from_list(
        lists:map(
            fun(
                {FileName, #fileInfo{
                    votes = Votes,
                    dotSet = DotSet,
                    fileHash = FileHash
                }}
            ) ->
                ParsedVotes = maps:from_list(
                    lists:map(
                        fun(
                            {Id, #voteInfo{
                                sum = Sum,
                                count = Count
                            }}
                        ) ->
                            {Id, {Sum, Count}}
                        end,
                        Votes
                    )
                ),
                ParsedDotSet = maps:from_keys(
                    lists:map(
                        fun(
                            #dotPair{
                                id = IDPair,
                                version = VersionPair
                            }
                        ) ->
                            {IDPair, VersionPair}
                        end,
                        DotSet
                    ),
                    true
                ),
                {FileName, {FileHash, ParsedVotes, ParsedDotSet}}
            end,
            Files
        )
    ),
    ParsedGroupUsers = maps:from_list(
        lists:map(
            fun(
                {Name, #groupInfo{
                    dotSet = DotSet
                }}
            ) ->
                ParsedDotSet = maps:from_keys(
                    lists:map(
                        fun(
                            #dotPair{
                                id = IDPair,
                                version = VersionPair
                            }
                        ) ->
                            {IDPair, VersionPair}
                        end,
                        DotSet
                    ),
                    true
                ),
                {Name, ParsedDotSet}
            end,
            GroupUsers
        )
    ),
    SessionLoop !
        {
            {put_album, UserName, {
                {ParsedFiles, ParsedGroupUsers, maps:from_list(VV)}, maps:from_list(Votetable)
            }},
            self()
        },
    session_user(Sock, SessionLoop, UserName).

session_user(Sock, SessionLoop, UserName) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            io:format("~p disconnected without saving!~n", [UserName]),
            error;
        {tcp, _, Msg} ->
            Message = message:decode_msg(Msg, 'Message'),
            session_message_handler(
                Message#'Message'.type, Message#'Message'.msg, Sock, SessionLoop, UserName
            );
        Msg ->
            session_user_handler(Msg, Sock, SessionLoop, UserName)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Authenticated %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Data, Sock) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, Data).

auth_user_handler(Status, Sock, MainLoop, UserName) when
    Status =:= get_album_no_permission;
    Status =:= get_album_already_in_session
->
    send_reply(atom_to_list(Status), Sock),
    auth_user(Sock, MainLoop, UserName);
auth_user_handler({new_server, IP, PORT, MainLoop}, Sock, MainLoop, UserName) ->
    Data = message:encode_msg(#'Message'{
        type = 2,
        msg =
            {m8, #newServer{
                ip = IP,
                port = PORT
            }}
    }),
    send(Data, Sock),
    auth_user(Sock, MainLoop, UserName);
auth_user_handler({Status, MainLoop}, Sock, MainLoop, UserName) when
    Status =:= get_album_no_permission;
    Status =:= get_album_error
->
    Data = message:encode_msg(#'Message'{
        type = 5,
        msg =
            {m3, #sessionStart{
                status = atom_to_list(Status)
            }}
    }),
    send(Data, Sock),
    auth_user(Sock, MainLoop, UserName);
auth_user_handler({Status, MainLoop}, Sock, MainLoop, UserName) when
    Status =:= create_album_error;
    Status =:= create_album_ok
->
    send_reply(atom_to_list(Status), Sock),
    auth_user(Sock, MainLoop, UserName);
auth_user_handler(
    {get_album_ok, {Id, {Files, GroupUsers, VV} = _Crdt, SessionPeers, Votetable}, SessionLoop},
    Sock,
    _,
    UserName
) ->
    io:format("~p~n", [SessionPeers]),
    Data = message:encode_msg(#'Message'{
        type = 5,
        msg =
            {m3, #sessionStart{
                id = Id,
                crdt = #crdt{
                    versionVector = maps:to_list(VV),
                    files = maps:to_list(
                        maps:map(
                            fun(_, {Hash, VoteMap, DotSet}) ->
                                #fileInfo{
                                    votes = maps:to_list(
                                        maps:map(
                                            fun(_, {Sum, Count}) ->
                                                #voteInfo{
                                                    sum = Sum,
                                                    count = Count
                                                }
                                            end,
                                            VoteMap
                                        )
                                    ),
                                    dotSet = lists:map(
                                        fun({IdDS, VersionDS}) ->
                                            #dotPair{
                                                id = IdDS,
                                                version = VersionDS
                                            }
                                        end,
                                        maps:keys(DotSet)
                                    ),
                                    fileHash = Hash
                                }
                            end,
                            Files
                        )
                    ),
                    groupUsers = maps:to_list(
                        maps:map(
                            fun(_, DotSet) ->
                                #groupInfo{
                                    dotSet = lists:map(
                                        fun({IdGDS, VersionGDS}) ->
                                            #dotPair{
                                                id = IdGDS,
                                                version = VersionGDS
                                            }
                                        end,
                                        maps:keys(DotSet)
                                    )
                                }
                            end,
                            GroupUsers
                        )
                    ),
                    id = Id
                },
                sessionPeers = maps:to_list(
                    maps:map(
                        fun(_, {IP, Port, IdSP}) ->
                            #peerInfo{
                                ip = IP,
                                port = Port,
                                id = IdSP
                            }
                        end,
                        SessionPeers
                    )
                ),
                voteTable = maps:to_list(Votetable),
                status = "get_album_ok"
            }}
    }),
    send(Data, Sock),
    session_user(Sock, SessionLoop, UserName);
auth_user_handler(_, Sock, MainLoop, UserName) ->
    auth_user(Sock, MainLoop, UserName).

auth_message_handler(
    logout,
    {m5, #reply_message{}},
    Sock,
    MainLoop,
    UserName
) ->
    MainLoop ! {log_out, UserName},
    Data = message:encode_msg(#'Message'{
        type = 11,
        msg =
            {m9, #log_out{}}
    }),
    send(Data, Sock),
    user(Sock, MainLoop);

auth_message_handler(create, {m2, #album{albumName = AlbumName}}, Sock, MainLoop, UserName) ->
    MainLoop ! {{create_album, UserName, AlbumName}, self()},
    auth_user(Sock, MainLoop, UserName);
auth_message_handler(
    get, {m2, #album{albumName = AlbumName, port = PORT}}, Sock, MainLoop, UserName
) ->
    {ok, {IP, _}} = inet:peername(Sock),
    MainLoop !
        {
            {get_album, UserName, string:join([integer_to_list(I) || I <- tuple_to_list(IP)], "."),
                PORT, AlbumName},
            self()
        },
    auth_user(Sock, MainLoop, UserName);
auth_message_handler(
    _, _, Sock, MainLoop, UserName
) ->
    auth_user(Sock, MainLoop, UserName).

auth_user(Sock, MainLoop, UserName) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            MainLoop ! {log_out, UserName};
        {tcp, _, Msg} ->
            Message = message:decode_msg(Msg, 'Message'),
            auth_message_handler(
                Message#'Message'.type, Message#'Message'.msg, Sock, MainLoop, UserName
            );
        Msg ->
            auth_user_handler(Msg, Sock, MainLoop, UserName)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Not Authenticated %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_reply(Status, Sock) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    Data = message:encode_msg(#'Message'{
        type = 7,
        msg =
            {m5, #reply_message{
                status = Status
            }}
    }),
    gen_tcp:send(Sock, Data).

user_handler({login_ok, UserName, DataServers, MainLoop}, Sock, MainLoop) ->
    Data = message:encode_msg(#'Message'{
        type = 2,
        msg =
            {m6, #login_reply{
                status = "login_ok",
                dataServers = lists:map(
                    fun({IP, PORT}) ->
                        #peerInfo{
                            ip = IP,
                            port = integer_to_list(PORT)
                        }
                    end,
                    DataServers
                )
            }}
    }),
    send(Data, Sock),
    auth_user(Sock, MainLoop, UserName);
user_handler({login_error, MainLoop}, Sock, MainLoop) ->
    Data = message:encode_msg(#'Message'{
        type = 2,
        msg =
            {m6, #login_reply{
                status = "login_error"
            }}
    }),
    send(Data, Sock),
    user(Sock, MainLoop);
user_handler({Status, MainLoop}, Sock, MainLoop) when
    Status =:= login_error;
    Status =:= register_ok;
    Status =:= register_error
->
    send_reply(atom_to_list(Status), Sock),
    user(Sock, MainLoop);
user_handler(_, Sock, MainLoop) ->
    user(Sock, MainLoop).

message_handler(
    register, {m1, #registerLoginFormat{userName = UserName, password = Password}}, Sock, MainLoop
) ->
    MainLoop ! {{register, {UserName, Password}}, self()},
    user(Sock, MainLoop);
message_handler(
    login, {m1, #registerLoginFormat{userName = UserName, password = Password}}, Sock, MainLoop
) ->
    MainLoop ! {{login, {UserName, Password}}, self()},
    user(Sock, MainLoop);
message_handler(
    _, _, Sock, MainLoop
) ->
    user(Sock, MainLoop).

user(Sock, MainLoop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            ok;
        {tcp, _, Msg} ->
            Message = message:decode_msg(Msg, 'Message'),
            io:format("~p~n", [Message]),
            message_handler(Message#'Message'.type, Message#'Message'.msg, Sock, MainLoop);
        Msg ->
            user_handler(Msg, Sock, MainLoop)
    end.
