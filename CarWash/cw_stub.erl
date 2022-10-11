-module(cw).
-compile(export_all).

machine() ->
    receive
       {From,permToProcess} ->
	    timer:sleep(rand:uniform(1000)), % processing
	    From!{doneProcessing},
	    machine()
    end.

waitForMachine(N) ->
    N!{self(), permToProcess},
    receive
        {doneProcessing},
        ok
    end.

acquireStation(N) ->
    whereis(control_center)!{self(),acquire,N},
    receive
        {ok} -> {ok};
        {notok} -> acquireStation(N)
    end.


releaseStation(N) ->
    whereis(control_center)!{self(),release,N}.
    
controlCenterLoop(S0,S1,S2) ->
    receive
        {From, acquire, N} ->
            case N of
                0 -> case S0 of
                        0 -> From!{notok};
                        1 -> From!{ok};
                    end
                1 -> case S1 of
                        0 -> From!{notok};
                        1 -> From!{ok};
                    end
                2 -> case S2 of
                        0 -> From!{notok};
                        1 -> From!{ok};
                    end
            end
        {From, release, N} ->
            case N of
                0 -> controlCenterLoop(1, S1, S2);
                1 -> controlCenterLoop(S0, 1, S2);
                2 -> controlCenterLoop(S0, S2, 1);
            end
    end.

car(M0,M1,M2) ->
    getStation(0),
    waitForMachine(M0),
    getStation(1),
    releaseStation(0),

    waitForMachine(M1),
    getStation(2),
    releaseStation(1),

    waitForMachine(M2),
    releaseStation(2).
  
start() ->
    M0 = spawn(fun machine/0),
    M1 = spawn(fun machine/0),
    M2 = spawn(fun machine/0),
    register(control_center,spawn(?MODULE,controlCenterLoop,[1,1,1])),
    [ spawn(?MODULE,car,[M0,M1,M2]) || _ <- lists:seq(1,20) ].

