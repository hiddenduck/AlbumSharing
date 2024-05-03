-module(client). %% For testing purposes ONLY
-export([start/1]).
-include("proto_generated/message.hrl").

start(Port) ->
    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [{active, false}]),
    io:format("Connected to server.~n"),
    Username = "testuser",
    Password = "password123",
    Data = message:encode_msg(#'Message'{
        type=0,
        msg={m1, #registerLoginFormat{
            userName = Username, 
            password = Password
        }}
    }),
    gen_tcp:send(Sock, Data),
    receive_response(Sock),
    gen_tcp:close(Sock).

receive_response(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            Message = message:decode_msg(Data, 'Message');
        {error, Reason} ->
            io:format("Error receiving response: ~p~n", [Reason])
    end.