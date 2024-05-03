-module(user_logic).
-export([user/2]).
-define(ACTIVE_TIMES, 10).
-include("proto_generated/message.hrl").

% Authenticated

%todo, meter args

auth_user_handler({tcp, _, "logout\n"}, _, MainLoop) ->
    MainLoop ! {{log_out}, self()};
auth_user_handler({tcp, _, Msg}, Sock, MainLoop) ->
    case string:split(Msg, ":", all) of
        [Action, AlbumName] when Action =:= "create_album"; Action =:= "get_album" ->
            MainLoop ! {{list_to_atom(Action), AlbumName}, self()},
            user(Sock, MainLoop);
        _ ->
            gen_tcp:send(Sock, "Incorrect Format\n")
    end;
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

auth_user(Sock, MainLoop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            MainLoop ! {{log_out}, self()};
        Msg ->
            auth_user_handler(Msg, Sock, MainLoop)
    end.

% Before Authentication
message_handler(
    register, {m1, #registerLoginFormat{userName = UserName, password = Password}}, Sock, MainLoop
) ->
    MainLoop ! {{register, {UserName, Password}}, self()},
    user(Sock, MainLoop);
message_handler(
    login, #registerLoginFormat{userName = UserName, password = Password}, Sock, MainLoop
) ->
    MainLoop ! {{login, {UserName, Password}}, self()},
    user(Sock, MainLoop).

user_handler({login_ok, MainLoop}, Sock, MainLoop) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    Data = message:encode_msg(#'Message'{
        type = 7,
        msg =
            {m5, #reply{
                status = "login_ok"
            }}
    }),
    gen_tcp:send(Sock, Data),
    auth_user(Sock, MainLoop);

user_handler({Status, MainLoop}, Sock, MainLoop) when
    Status =:= login_error;
    Status =:= register_ok;
    Status =:= register_error
->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    Data = message:encode_msg(#'Message'{
        type = 7,
        msg =
            {m5, #reply{
                status = atom_to_list(Status)
            }}
    }),
    gen_tcp:send(Sock, Data),
    user(Sock, MainLoop);

user_handler(_, Sock, MainLoop) ->
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
