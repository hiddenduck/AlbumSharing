-module(data_loop).
-export([start/3]).


start(Port, Central, MainLoop) ->
    register(?MODULE, spawn(fun() -> start_loop(Port, Central, MainLoop) end)),
    ok.

start_loop(Port, Central, MainLoop) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, once}, {packet, raw},
                                      {reuseaddr, true}]),

    Loop = spawn(fun() -> loop(MainLoop, []) end),
    spawn(fun() -> acceptor(LSock, Loop) end),

    receive
        {stop, Central} -> ok
    end.

acceptor(LSock, Loop) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Loop) end),
    data_server(Sock, Loop).

binary_search([], _, _, _) -> {-1,0};
binary_search(Servers, Hash, _, Right) when element(2, lists:nth(Right, Servers)) < Hash ->
    {Right, Right + 1};
binary_search([{_, _, H} | _], Hash, _, _) when H >= Hash ->
    {-1, 1};
binary_search(Servers, Hash, Left, Right) ->
    binary_search_aux(Servers, Hash, Left, Right).
binary_search_aux(_, _, Left, Right) when Right - Left =< 1 ->
    {Left, Right};
binary_search_aux(Servers, Hash, Left, Right) ->
    Mid = (Left + Right)/2,
    {_, _, Val} = lists:nth(Mid, Servers),
    if 
        Val >= Hash -> binary_search_aux(Servers, Hash, Left, Mid);
        true -> binary_search_aux(Servers, Hash, Mid, Right)
    end.

handler({join, IP, PORT}, {MainLoop, DataServers}, From) -> % Port is also a string
    Hash = crypto:hash(<<IP/binary, PORT/binary>>),
    {Left, Right} = binary_search(DataServers, Hash, 0, lists:length(DataServers)-1),
    {FirstHalf, SecondHalf} = lists:split(Right, DataServers),
    FirstHalf ++ [{IP, PORT, Hash}] ++ SecondHalf.

loop(MainLoop, DataServers) ->
    receive
        {Msg, From} ->
            loop(MainLoop, handler(Msg, {MainLoop, DataServers}, From))
    end.

-ifdef(comment).
message_handler(
    register, {m1, #registerLoginFormat{userName = UserName, password = Password}}, Sock, MainLoop
) ->
    MainLoop ! {{register, {UserName, Password}}, self()},
    data_server(Sock, MainLoop);
message_handler(
    login, #registerLoginFormat{userName = UserName, password = Password}, Sock, MainLoop
) ->
    MainLoop ! {{login, {UserName, Password}}, self()},
    data_server(Sock, MainLoop);
message_handler(
    _, _, Sock, MainLoop
) ->
    data_server(Sock, MainLoop).
-endif().

data_server(Sock, Loop) ->
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            ok
        %{tcp, _, Msg} ->
         %   Message = message:decode_msg(Msg, 'Message'),
          %  message_handler(Message#'Message'.type, Message#'Message'.msg, Sock, MainLoop);
        %Msg ->
         %   user_handler(Msg, Sock, MainLoop)
    end.
    

