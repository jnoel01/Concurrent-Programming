%% Jessica Noel
%% Concurrent Programming

-module(shipping).
-compile(export_all).
-compile(nowarn_export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
    case lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships) of
        false -> 
            error;
        _ -> 
            lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships)
    end.

get_container(Shipping_State, Container_ID) ->
    case lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers) of
        false -> 
            error;
        _ -> 
            lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers)
    end.

get_port(Shipping_State, Port_ID) ->
    case lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports) of
        false -> 
            error;
        _ -> 
            lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports)
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    Same_Port = lists:filter(fun ({Port, _Dock, _Ship}) -> 
        Port == Port_ID end, Shipping_State#shipping_state.ship_locations),
    lists:map(fun({_Port, Dock, _Ship}) -> 
        Dock end, Same_Port).

get_ship_location(Shipping_State, Ship_ID) ->
    case lists:keyfind(Ship_ID, 3, Shipping_State#shipping_state.ship_locations) of
      {Port, Dock, _Ship} -> 
        {Port,Dock};
      _ -> 
        error
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    All_Containers = [get_container(Shipping_State, Container_ID) || 
        Container_ID <- Container_IDs],
    case lists:member(error, All_Containers) of 
      true -> error;
      false ->
        lists:sum([Container#container.weight ||
            Container <- All_Containers])
    end.  

get_ship_weight(Shipping_State, Ship_ID) ->
    case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of
        {ok, ContainerID} ->
            get_container_weight(Shipping_State, ContainerID);
        _ -> 
            error
    end.

%% Helper for loadship func to remove
delete(X, N) ->
    case N of
        [] -> X;
        [H | T] ->
            delete(lists:delete(H, X), T)
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {P, _D} = get_ship_location(Shipping_State, Ship_ID),
    Max_Capacity = (get_ship(Shipping_State, Ship_ID))#ship.container_cap,
    case Container_IDs of
        [] -> Shipping_State;
        [_H | _T] ->
            {ok, Inventory_Ship} = maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory),
            {ok, Inventory_Port} = maps:find(P, Shipping_State#shipping_state.port_inventory),
            case(length(Inventory_Ship) + length(Container_IDs) =< Max_Capacity) and is_sublist(Inventory_Port, Container_IDs) of
                true ->
                    Updated_Inv_Ship = lists:merge(Inventory_Ship, Container_IDs),
                    Updated_Inv_Port = lists:filter(fun (X) ->
                        not lists:member(X, Container_IDs) end, Inventory_Port),
                    #shipping_state {
                        ships = Shipping_State#shipping_state.ships,
                        containers = Shipping_State#shipping_state.containers,
                        ports = Shipping_State#shipping_state.ports,
                        ship_locations = Shipping_State#shipping_state.ship_locations,
                        ship_inventory = maps:put(Ship_ID ,Updated_Inv_Ship,Shipping_State#shipping_state.ship_inventory),
                        port_inventory = maps:put(Container_IDs, Updated_Inv_Port, Shipping_State#shipping_state.port_inventory)
                    };
                false -> 
                    error
            end;
        _ ->
            error
    end.


unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {Port, _Dock} = get_ship_location(Shipping_State, Ship_ID),
    PortCap = (get_port(Shipping_State, Port))#port.container_cap,
    {ok, Inventory_Ship} = maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory),
    {ok, Inventory_Port} = maps:find(Port, Shipping_State#shipping_state.port_inventory),
    case(length(Inventory_Port) + length(Container_IDs) =< PortCap) and is_sublist(Inventory_Ship, Container_IDs) of
        true ->
            Updated_Inv_Ship = lists:filter(fun(X) ->
                not lists:member(X, Container_IDs) end, Inventory_Ship),
                Updated_Inv_Port =  lists:merge(Inventory_Port, Container_IDs),
                #shipping_state{
                    ships = Shipping_State#shipping_state.ships,
                    containers = Shipping_State#shipping_state.containers,
                    ports = Shipping_State#shipping_state.ports,
                    ship_locations = Shipping_State#shipping_state.ship_locations,
                    ship_inventory = maps:put(Ship_ID ,Updated_Inv_Ship,Shipping_State#shipping_state.ship_inventory),
                    port_inventory = maps:put(Port, Updated_Inv_Port, Shipping_State#shipping_state.port_inventory)
                };
        false ->
            error
    end.

%% Moved func after unload ship so I can call unload ship on each container
unload_ship_all(Shipping_State, Ship_ID) ->
    unload_ship(Shipping_State, Ship_ID, maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory)).

setsail_helper(N, PortID, DockID, ShipID)->
    case N of
        [] -> [];
        [{Portt, Dock, Shipp} | X]  ->
            if Shipp == ShipID ->
                [{PortID, DockID, Shipp}]++setsail_helper(X, PortID, DockID, ShipID);

                true ->
                [{Portt, Dock, Shipp}]++setsail_helper(X, PortID, DockID, ShipID)
            end
    end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    case get_ship_location(Shipping_State, Ship_ID) of 
        error -> error;
        {PortID, DockID} ->
            if ((PortID == Port_ID) and (DockID == Dock)) ->
                Shipping_State;
                true ->
                    case is_sublist(get_occupied_docks(Shipping_State, Port_ID), [Dock]) of
                        true -> error;
                        false ->
                            case setsail_helper(Shipping_State#shipping_state.ship_locations, Port_ID, Dock, Ship_ID) of
                                [] -> error;
                                Z -> Z,
                                Ship_State = Shipping_State#shipping_state{ship_locations = Z},
                                Ship_State
                            end
                    end
            end
    end.

%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).

%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).

%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).

print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
