-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
	Chats = maps:keys(State#serv_st.chatrooms),
	%% Chat name taken
	case lists:member(ChatName, Chats) of
		true ->
			UpdatedChats = State#serv_st.chatrooms,
			PrevRegistration = maps:get(ChatName,State#serv_st.registrations),
			UpdateRegistration = maps:update(ChatName, lists:append([ClientPID], PrevRegistration),
			State#serv_st.registrations);
		%%  Make a new chat
		%% Tell the chatroom the client is joining the chatroom
		%% Update local record of registered clients
		false ->
			NewMessage = spawn(chatroom, start_chatroom, [ChatName]),
			UpdatedChats = maps:put(ChatName, NewMessage, State#serv_st.chatrooms),
			UpdateRegistration = maps:put(ChatName, [ClientPID], State#serv_st.registrations)
	end,
	{ ok, ChatMsg } = maps:find(ChatName, UpdatedChats),
	{ ok, NickT } = maps:find(ClientPID, State#serv_st.nicks),
    ChatMsg!{ self(), Ref, register, ClientPID, NickT },
    #serv_st{
		nicks = State#serv_st.nicks,
		registrations = UpdateRegistration,
		chatrooms = UpdatedChats
    }.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    ChatPID = maps:get(ChatName, State#serv_st.chatrooms), 
	PrevRegistration = maps:get(ChatName, State#serv_st.registrations),
    UpdatedRegistration = State#serv_st{
		registrations = maps:update(ChatName, lists:delete(ClientPID, PrevRegistration),
		State#serv_st.registrations)
	},
    ChatPID!{ self(), Ref, unregister, ClientPID },
    ClientPID!{ self(), Ref, ack_leave },
    UpdatedRegistration.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    Nicknames = maps:values(State#serv_st.nicks),
	%% Can't use that nickname cases:
	%% Nickname taken by other
	%% Already that nickname
	case lists:member(NewNick, Nicknames) of
	true ->
		ClientPID!{self(), Ref, err_nick_used},
		State;
	false ->
		%% Just update the record of nicknames by pointing the client PID to new nick
		%% Update all chatrooms for the updated nickname
		Chats = maps:filter(fun(_X, Clients) ->
		lists:member(ClientPID, Clients)
		end,
		State#serv_st.registrations),
		ChatPIDs = maps:filter(fun(X, _PID) ->
			lists:member(X, maps:keys(Chats))
			end,
			State#serv_st.chatrooms),
		_Temp = maps:map(fun(_X , ChatPID) ->
			ChatPID!{ self(), Ref, update_nick, ClientPID, NewNick }, ChatPID end, ChatPIDs),
			NewNicks = maps:update(ClientPID, NewNick, State#serv_st.nicks),
			ClientPID!{self(), Ref, ok_nick},
			#serv_st{
				nicks = NewNicks,
				registrations = State#serv_st.registrations,
				chatrooms = State#serv_st.chatrooms
			}
		end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    Chats = maps:filter(fun(_X, Clients) ->
		lists:member(ClientPID, Clients) end, State#serv_st.registrations),
    ChatPIDs = maps:filter(fun(X, _PID) ->
		lists:member(X, maps:keys(Chats)) end, State#serv_st.chatrooms),
    	_Temp = maps:map(fun(_, ChatPID) ->
       		ChatPID!{self(), Ref, upregister, ClientPID},  ChatPID end, ChatPIDs),
      		UpdatedRegistrations = maps:map(fun(_X, Clients) ->
				case lists:member(ClientPID, Clients) of
				true ->
					lists:delete(ClientPID, Clients);
				false ->
					Clients
					end
				end, State#serv_st.registrations),

				ClientPID!{self(), Ref, ack_quit},
				#serv_st{
					nicks = maps:remove(ClientPID,State#serv_st.nicks),
					registrations = UpdatedRegistrations,
					chatrooms = State#serv_st.chatrooms
				}.
