-module(basic).
-compile(export_all).


% start(W,M) ->
%     S = spawn(?MODULE, server, [0]),
%     [spawn(?MODULE, patriots, [S]) || _ <- lists:seq(1,W)],
%     [spawn(?MODULE, jets, [S]) || _ <- lists:seq(1,M)].

% patriots(S) ->
%     S!{self(), patriots}.

% jets(S) ->
%     S!{self(), jets},
%     receive
%         {S, ok} ->
%             ok
%     end.

% server(Patriots) ->
%     receive 
%         {_From, patriots} ->
%             server(Patriots+1);
%         {From, jets} when Patriots >= 2 ->
%             From!{self(), ok},
%             server(Patriots-2)
%     end.





% %% Turnstile Example 
% start(N) ->
%     C = spawn(?MODULE, counter_server, [0]),
%     [ spawn(?MODULE, turnstile, [C,50]) || _ <- lists:seq(1,N)],
%     C.

% counter_server(State) ->
%     receive
%         {_From, _N, bump} ->
%             counter_server(State + 1);
%         {read, From} ->
%             From!{State}
%     end.


% turnstile(C,N) ->
%     case N > 0 of
%         true -> C!{self(), N, bump},
%                 turnstile(C, N-1);
%         false -> {done}
%     end.



% %% Implement a Server that concatenates strings
% start() ->
%     S = spawn(?MODULE, server, []),
%     [ spawn(?MODULE, client, [S]) || _ <- lists:seq(1,100)].

% client(S) ->
%     S!{start, self()},
%     S!{add, "h", self()},
%     S!{add, "e", self()},
%     S!{add, "l", self()},
%     S!{add, "l", self()},
%     S!{add, "o", self()},
%     S!{done, self()},
%     receive
%         {S, Str} ->
%             io:format("Done: ~p~s~n", [self(),Str])
%     end.

% server_ex2(Str) ->
%     receive
%         {start, _From} ->
%             server_ex2("");
%         {add, S, _From} ->
%             server_ex2(Str ++ S);
%         {done, From} ->
%             From!{self(), Str}
%     end.


%% Timer Exercise 4
% start_timer(T) ->
%     S = spawn(?MODULE, timer, [T,[self()]]),
%     [spawn(?MODULE, client_Ex5, [S]) || _ <- lists:seq(1,5)].

% timer(N, S) ->
%     timer:sleep(N),
%     [ Pid!{tick} || Pid <- S],
%     receive
%         {From, register} ->
%             timer(N, S ++ From)
%     end,
%     timer(N, S).
            

% client_loop() ->
%     receive
%         {tick} ->
%             io:format("Done", self()),
%             client_loop()
%     end.

% client_Ex5(S) ->
%     S!{self(), register},
%     client_loop().
    


%Exercise 8
start() ->
    S = spawn(fun server/0),
    [ spawn(?MODULE, client, [S]) || _ <- lists: seq(1,N)].

server() ->
    receive
        {From, Ref, start} ->
            Servlet = spawn(?MODULE, servlet_loop, [rand:uniform(100)]),
            From!{ok, Servlet, Ref},
            server()
    end.

serlvet_loop(N) ->
    receive
        {From, Guess, Ref, guess} ->
            case Guess == N of
                true ->
                    From!{self(), Ref, correct};
                false -> 
                    From!{self(), Ref, wrong},
                    servlet_loop(N),
            end
    end.