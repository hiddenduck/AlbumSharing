-module(user_logic).
-export([user/2]).

handler({line, {Self, Data}}, Sock, MainLoop) when Self =:= self() ->
    inet:setopts(Sock, [{active, once}]),
    gen_tcp:send(Sock, Data),
    user(Sock, MainLoop);

handler({line, {_, Data}}, Sock, MainLoop) ->
    gen_tcp:send(Sock, Data),
    user(Sock, MainLoop);

handler({tcp, _, Data}, Sock, MainLoop) ->
    MainLoop ! {line, {self(), Data}},
    user(Sock, MainLoop);

handler({tcp_closed, _}, _, MainLoop) ->
    MainLoop ! {leave, self()};

handler({tcp_error, _, _}, _, MainLoop) ->
    MainLoop ! {leave, self()}.

user(Sock, MainLoop) ->
    receive
        Msg -> handler(Msg, Sock, MainLoop)
    end.
