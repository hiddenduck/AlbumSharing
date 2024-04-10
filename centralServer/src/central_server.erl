-module(central_server).
-export([start/1, stop/0]).

start(Port) ->
    register(?MODULE, spawn(fun() -> start_server(Port) end)),
    ok.

stop() ->
    ?MODULE ! stop.

% Function that holds the state creation logic
createState() ->
    {
        % All Users
        #{}, % Users -> Password

        % Album Metadata
        {
            #{}, % Users -> 
            #{}  % File  -> (Rating -> {count, [Users]})
        }
    }.

start_server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),

    MainLoop = spawn(fun() -> main_loop:mainLoop(createState()) end),
    spawn(fun() -> acceptor(LSock, MainLoop) end),

    receive
        stop -> ok
    end.

acceptor(LSock, MainLoop) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, MainLoop) end),
    MainLoop ! {enter, self()},
    user_logic:user(Sock, MainLoop).
