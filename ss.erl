-module(ss).
-compile(nowarn_export_all).
-compile(export_all).

%%Client get PID(S) from server
start() ->
    S = spawn(?MODULE, server, []),
    [ spawn(?MODULE, client, [S]) || _ <- lists:seq(1,100000) ].

client_loop(Servlet) ->
    Servlet!{add, "h", self()},
    Servlet!{add, "e", self()},
    Servlet!{add, "l", self()},
    Servlet!{add, "l", self()},
    Servlet!{add, "o", self()},
    Servlet!{done, self()}, 
    receive
    {_S, Str} ->
        io:format("Done: ~p~s~n", [self(), Str])
    end.


client(Server) ->
    Server!{start, self()},
    receive
        {ok, S} ->
            client_loop(S)
    end.

server() ->
    receive
        {start, PID} ->
            S = spawn(?MODULE, servlet, [""]),
            PID!{ok, S},
            server()
    end.

servlet(Str) ->
    receive
        {add, S, _PID} ->
            servlet(Str ++ S);
        {done, PID} ->
            PID!{self(), Str}
        end.
