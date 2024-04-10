-module(user_logic).
-export([user/2]).
-define(ACTIVE_TIMES, 10).

% Authenticated

%todo, meter args
auth_user_handler({tcp, _, "create_album\n"}, Sock, MainLoop) ->
    MainLoop ! {create_album, self()},
    auth_user(Sock, MainLoop);

auth_user_handler({tcp, _, "get_album_replica\n"}, Sock, MainLoop) ->
    MainLoop ! {get_album_replica, self()},
    auth_user(Sock, MainLoop);

auth_user_handler({tcp, _, "logout\n"}, _, MainLoop) ->
    MainLoop ! {log_out, self()};

auth_user_handler({Info, MainLoop}, Sock, MainLoop) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, atom_to_list(Info)),
    auth_user(Sock, MainLoop);

auth_user_handler(_, Sock, MainLoop) ->
    auth_user(Sock, MainLoop).

auth_user(Sock, MainLoop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            MainLoop ! {leave, self()};
        
        Msg -> auth_user_handler(Msg, Sock, MainLoop)
    end.


% Before Authentication

user_handler({tcp, _, Msg}, Sock, MainLoop)->
    case string:split(Msg, ":", all) of
        [Action, UserName, Password] when Action =:= "register:"; Action =:= "login:"->
            MainLoop ! {list_to_atom(lists:droplast(Action)), {UserName, lists:droplast(Password)}, self()},
            user(Sock, MainLoop);

        _ ->
            gen_tcp:send(Sock, "Incorrect Format")
    end;

user_handler({Info, MainLoop}, Sock, MainLoop) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, atom_to_list(Info)),
    auth_user(Sock, MainLoop);

user_handler({tcp_closed, _}, _, MainLoop) ->
    MainLoop ! {leave, self()};

user_handler({tcp_error, _, _}, _, MainLoop) ->
    MainLoop ! {leave, self()};

user_handler(_, Sock, MainLoop) ->
    user(Sock, MainLoop).

user(Sock, MainLoop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed;TCP_Info =:= tcp_error ->
            MainLoop ! {leave, self()};
        
        Msg -> user_handler(Msg, Sock, MainLoop)
    end.
