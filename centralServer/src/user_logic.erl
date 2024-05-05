-module(user_logic).
-export([user/2]).
-define(ACTIVE_TIMES, 10).
-include("proto_generated/message.hrl").

% Authenticated

auth_user_handler({Status, MainLoop}, Sock, MainLoop) when
    Status =:= create_album_error;
    Status =:= create_album_ok;
    Status =:= get_album_no_permission;
    Status =:= get_album_error
->
    send_reply(atom_to_list(Status), Sock),
    auth_user(Sock, MainLoop);

auth_user_handler({get_album_ok, AlbumData, MainLoop}, Sock, MainLoop) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, atom_to_list(get_album_ok) ++ maps:to_list(AlbumData) ++ "\n"),
    auth_user(Sock, MainLoop);

auth_user_handler({Info, MainLoop}, Sock, MainLoop) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, atom_to_list(Info) ++ "\n"),
    auth_user(Sock, MainLoop);

auth_user_handler(_, Sock, MainLoop) ->
    auth_user(Sock, MainLoop).

auth_message_handler(album, {m1, #album{albumName = AlbumName}}, Sock, MainLoop) ->
    MainLoop ! {{create_album, AlbumName}, self()},
    auth_user(Sock, MainLoop).

auth_user(Sock, MainLoop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            MainLoop ! {{log_out}, self()};

        {tcp, _, Msg} ->
            Message = message:decode_msg(Msg, 'Message'),
            auth_message_handler(Message#'Message'.type, Message#'Message'.msg, Sock, MainLoop);

        Msg ->
            auth_user_handler(Msg, Sock, MainLoop)
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

user_handler({login_ok, MainLoop}, Sock, MainLoop) ->
    send_reply("login_ok", Sock),
    auth_user(Sock, MainLoop);

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
            MainLoop ! {{log_out}, self()};
        {tcp, _, Msg} ->
            Message = message:decode_msg(Msg, 'Message'),
            message_handler(Message#'Message'.type, Message#'Message'.msg, Sock, MainLoop);
        Msg ->
            user_handler(Msg, Sock, MainLoop)
    end.
