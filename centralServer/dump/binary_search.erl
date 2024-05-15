-module(binary_search).
-export([binary_search/4]).

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