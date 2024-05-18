-module(data_loop).
-export([start/3]).

start(Port, Central, MainLoop) ->
    register(?MODULE, self()),
    start_loop(Port, Central, MainLoop).

start_loop(Port, Central, MainLoop) ->
    Self = self(),
    {ok, LSock} = gen_tcp:listen(Port, [{active, once}, {packet, line},
                                      {reuseaddr, true}]),

    spawn(fun() -> acceptor(LSock, Self) end),
    loop(MainLoop, [], Central).

acceptor(LSock, Loop) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Loop) end),
    data_server(Sock, Loop).

binary_search(Servers, Hash, Left, Right) ->
    [FirstServer | _] = Servers,
    LastServer = lists:nth(Right, Servers),
    if
        element(3, FirstServer) >= Hash -> {LastServer, FirstServer, 0};
        element(3, LastServer) < Hash -> {LastServer, FirstServer, Right};
        true -> binary_search_aux(Servers, Hash, Left, Right)
    end.
binary_search_aux(Servers, _, Left, Right) when Right - Left =< 1 ->
    {lists:nth(Left, Servers), lists:nth(Right, Servers), Left};
binary_search_aux(Servers, Hash, Left, Right) ->
    Mid = (Left + Right) div 2,
    {_, _, Val} = lists:nth(Mid, Servers),
    if 
        Val >= Hash -> binary_search_aux(Servers, Hash, Left, Mid);
        true -> binary_search_aux(Servers, Hash, Mid, Right)
    end.

handler({join, IP, PORT}, {MainLoop, []}, From) -> 
    BinIP = list_to_binary(IP),
    Hash = crypto:hash(sha256, <<BinIP/binary, PORT/integer>>),
    From ! {Hash, self()},
    MainLoop ! {{addServer, IP, PORT, 0}, self()},
    [{IP, PORT, Hash}];

handler({join, IP, PORT}, {MainLoop, DataServers}, From) -> % Port is also a string
    BinIP = list_to_binary(IP),
    Hash = crypto:hash(sha256, <<BinIP/binary, PORT/integer>>),
    {InfServer, TopServer, IndexToAdd} = binary_search(DataServers, Hash, 1, length(DataServers)),
    {FirstHalf, SecondHalf} = lists:split(IndexToAdd, DataServers),
    From ! {InfServer, TopServer, Hash, self()},
    MainLoop ! {{addServer, IP, PORT, length(FirstHalf)}, self()},
    FirstHalf ++ [{IP, PORT, Hash}] ++ SecondHalf.

loop(MainLoop, DataServers, Central) ->
    receive
        {stop, Central} -> 
            ok;
        {Msg, From} ->
            loop(MainLoop, handler(Msg, {MainLoop, DataServers}, From), Central)
    end.

hash_to_hex(Hash) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Hash]).

data_server(Sock, Loop) ->
    {ok, {MyIP, _}} = inet:peername(Sock),
    receive
        {tcp, _, Msg} ->
            {Port,_} = string:to_integer(lists:droplast(Msg)),
            Loop ! {{join, string:join([integer_to_list(I) || I <- tuple_to_list(MyIP)], "."), Port}, self()}
    end,
    receive
        {Hash, Loop} ->
            gen_tcp:send(Sock, ""++";"++hash_to_hex(Hash)++":\n");
        {{_, _, InfHash}, {IP, PORT, _}, Hash, Loop} -> 
            gen_tcp:send(Sock, IP++";"++integer_to_list(PORT)++":"++hash_to_hex(Hash)++":"++hash_to_hex(InfHash)++":\n")
    end,
    receive
        {TCP_Info, _} when TCP_Info =:= tcp_closed; TCP_Info =:= tcp_error ->
            ok
    end.

