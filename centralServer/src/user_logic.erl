-module(user_logic).
-export([user/2]).
-define(ACTIVE_TIMES, 10).

% Authenticated

%todo, meter args

auth_user_handler({tcp, _, "logout\n"}, _, MainLoop) ->
    MainLoop ! {{log_out}, self()};

auth_user_handler({tcp, _, Msg}, Sock, MainLoop)->
    case string:split(Msg, ":", all) of
        [Action, AlbumName] when Action =:= "create_album"; Action =:= "get_album_replica"->
            MainLoop ! {{list_to_atom(Action), AlbumName}, self()},
            user(Sock, MainLoop);
    
        _ ->
            gen_tcp:send(Sock, "Incorrect Format\n")
    end;

auth_user_handler({Info, MainLoop}, Sock, MainLoop) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, atom_to_list(Info)++"\n"),
    auth_user(Sock, MainLoop);

auth_user_handler(_, Sock, MainLoop) ->
    auth_user(Sock, MainLoop).

auth_user(Sock, MainLoop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            MainLoop ! {{log_out}, self()};
        
        Msg -> auth_user_handler(Msg, Sock, MainLoop)
    end.


% Before Authentication

user_handler({tcp, _, Msg}, Sock, MainLoop)->
    io:format("~p~n", [Msg]),
    case string:split(Msg, ":", all) of
        [Action, UserName, Password] when Action =:= "register"; Action =:= "login"->
            MainLoop ! {{list_to_atom(Action), {UserName, lists:droplast(Password)}}, self()},
            user(Sock, MainLoop);

        _ ->
            gen_tcp:send(Sock, "Incorrect Format\n")
    end;

user_handler({Info, MainLoop}, Sock, MainLoop) ->
    inet:setopts(Sock, [{active, ?ACTIVE_TIMES}]),
    gen_tcp:send(Sock, atom_to_list(Info)++"\n"),
    auth_user(Sock, MainLoop);

user_handler(_, Sock, MainLoop) ->
    user(Sock, MainLoop).

user(Sock, MainLoop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed;TCP_Info =:= tcp_error ->
            MainLoop ! {{log_out}, self()};
        
        Msg -> user_handler(Msg, Sock, MainLoop)
    end.
