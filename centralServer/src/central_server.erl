-module(central_server).
-export([start/2, stop/0]).

start(Port, DataLoopPort) ->
    register(?MODULE, spawn(fun() -> start_server(Port, DataLoopPort) end)),
    ok.

stop() ->
    ?MODULE ! stop.

% Function that holds the state creation logic
createState() ->
    {
        % All Users
        #{}, % Users -> {isOnline, Password}
        
        % Album Room, String(album name) -> Pid, mapping the album name to the current session
        #{}
    }.

start_server(Port, DataLoopPort) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, once}, {packet, raw},
                                      {reuseaddr, true}]),
    
    MainLoop = spawn(fun() -> main_loop:mainLoop(createState(), self()) end),
    DataLoop = spawn(fun() -> data_loop:start(DataLoopPort, self(), MainLoop) end),
    MainLoop ! {DataLoop, self()},
    spawn(fun() -> acceptor(LSock, MainLoop) end),
    
    receive
        stop ->
            DataLoop ! {stop, self()},
            ok
    end.

acceptor(LSock, MainLoop) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, MainLoop) end),
    user_logic:user(Sock, MainLoop).
