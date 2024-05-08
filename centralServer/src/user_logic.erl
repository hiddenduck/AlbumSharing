-module(user_logic).
-export([user/2]).
-define(ACTIVE_TIMES, 10).
-include("proto_generated/message.hrl").

% In Session

session_user_handler({Status, SessionLoop}, Sock, SessionLoop, UserName) when
    Status =:= put_album_ok;
    Status =:= put_album_not_in_session;
    Status =:= put_album_no_permission
->
    send_reply(atom_to_list(Status), Sock),
    session_user(Sock, SessionLoop, UserName).

session_message_handler(quit, {m4, #quitMessage{crdt = Crdt, voteTable = Votetable}}, Sock, SessionLoop, UserName) ->
    SessionLoop ! {{put_album, UserName, {Crdt, Votetable}}, self()},
    session_user(Sock, SessionLoop, UserName).

session_user(Sock, SessionLoop, UserName) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            SessionLoop ! {log_out, UserName};

        {tcp, _, Msg} ->
            Message = message:decode_msg(Msg, 'Message'),
            session_message_handler(Message#'Message'.type, Message#'Message'.msg, Sock, SessionLoop, UserName);

        Msg ->
            session_user_handler(Msg, Sock, SessionLoop, UserName)
    end.

% Authenticated

send(Data, Sock) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, Data).

auth_user_handler(Status, Sock, MainLoop, UserName) when
    Status =:= get_album_no_permission;
    Status =:= get_album_already_in_session
->
    send_reply(atom_to_list(Status), Sock),
    auth_user(Sock, MainLoop, UserName);

auth_user_handler({Status, MainLoop}, Sock, MainLoop, UserName) when
    Status =:= create_album_error;
    Status =:= create_album_ok;
    Status =:= get_album_no_permission;
    Status =:= get_album_error;
    Status =:= put_album_ok;
    Status =:= put_album_not_in_session;
    Status =:= put_album_no_permission
->
    send_reply(atom_to_list(Status), Sock),
    auth_user(Sock, MainLoop, UserName);

auth_user_handler({get_album_ok, {Id, Crdt, SessionPeers, Votetable}, SessionLoop}, Sock, _, UserName) ->
    Data = message:encode_msg(#'Message'{
        type = 5,
        msg =
            {m3, #sessionStart{
                id = Id,
                crdt = Crdt,
                sessionPeers = SessionPeers,
                voteTable = Votetable
            }}
    }),
    send(Data, Sock),
    session_user(Sock, SessionLoop, UserName);

auth_user_handler(_, Sock, MainLoop, UserName) ->
    auth_user(Sock, MainLoop, UserName).

auth_message_handler(create, {m2, #album{albumName = AlbumName}}, Sock, MainLoop, UserName) ->
    MainLoop ! {{create_album, UserName, AlbumName}, self()},
    auth_user(Sock, MainLoop, UserName);

auth_message_handler(get, {m2, #album{albumName = AlbumName}}, Sock, MainLoop, UserName) ->
    MainLoop ! {{get_album, UserName, AlbumName}, self()},
    auth_user(Sock, MainLoop, UserName).

auth_user(Sock, MainLoop, UserName) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            MainLoop ! {log_out, UserName};

        {tcp, _, Msg} ->
            Message = message:decode_msg(Msg, 'Message'),
            auth_message_handler(Message#'Message'.type, Message#'Message'.msg, Sock, MainLoop, UserName);

        Msg ->
            auth_user_handler(Msg, Sock, MainLoop, UserName)
    end.

% Before Authentication
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

user_handler({login_ok, UserName, MainLoop}, Sock, MainLoop) ->
    send_reply("login_ok", Sock),
    auth_user(Sock, MainLoop, UserName);

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
    login, #registerLoginFormat{userName = UserName, password = Password}, Sock, MainLoop
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
            message_handler(Message#'Message'.type, Message#'Message'.msg, Sock, MainLoop);
        Msg ->
            user_handler(Msg, Sock, MainLoop)
    end.
