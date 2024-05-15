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
binary_search([FirstServer | Servers], Hash, Left, Right) ->
    LastServer = lists:nth(Right, Servers),
    if
        element(3, FirstServer) >= Hash -> {LastServer, FirstServer, 0};
        element(3, LastServer) < Hash -> {LastServer, FirstServer, Right};
        true -> binary_search_aux(Servers, Hash, Left, Right)
    end.
binary_search_aux(Servers, _, Left, Right) when Right - Left =< 1 ->
    {lists:nth(Left, Servers), lists:nth(Right, Servers), Left};
binary_search_aux(Servers, Hash, Left, Right) ->
    Mid = (Left + Right)/2,
    {_, _, Val} = lists:nth(Mid, Servers),
    if 
        Val >= Hash -> binary_search_aux(Servers, Hash, Left, Mid);
        true -> binary_search_aux(Servers, Hash, Mid, Right)
    end.

handler({join, IP, PORT}, {MainLoop, DataServers}, From) -> % Port is also a string
    Hash = crypto:hash(<<IP/binary, PORT/binary>>),
    {LeftServer, RightServer, IndexToAdd} = binary_search(DataServers, Hash, 1, lists:length(DataServers)),
    {FirstHalf, SecondHalf} = lists:split(IndexToAdd, DataServers),
    %warn server of its location
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
    

