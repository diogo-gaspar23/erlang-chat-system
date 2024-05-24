-module(router).
-export([init/2, start/1, start_link/2, terminate/0]).
-record(state, {chat_servers, % List of {MonitorPid, ServerName, ServerPid, RemoteMachine, State}
                remote_machines}).

ping_remote(RemoteMachine) ->
    net_adm:ping(RemoteMachine).

start(RouterMonitor) ->
    spawn(?MODULE, init, [RouterMonitor, #state{chat_servers=[],remote_machines=[]}]).
 
start_link(RouterMonitor, State) ->
    register(?MODULE, Pid = spawn_link(?MODULE, init, [RouterMonitor, State])),
    Pid.
 
terminate() ->
    ?MODULE ! shutdown.

init(RouterMonitor, PrevState = #state{}) ->
    UpdatedRemoteMachines = updateRemotes(PrevState#state.remote_machines),
    UpdatedState = monitor_state(PrevState#state{remote_machines=UpdatedRemoteMachines}),
    loop(RouterMonitor, UpdatedState).

loop(RouterMonitor, S = #state{}) ->
    receive
        {From, MsgRef, {add_server, Name, RemoteMachine}} ->
            NewState = handle_new_server(From, MsgRef, S, RemoteMachine, Name),
            RouterMonitor ! {self(), state_change, NewState},
            loop(RouterMonitor, NewState);
        {From, MsgRef, get_chat_servers} ->
            From ! {MsgRef, get_chat_servers(S#state.chat_servers)},
            loop(RouterMonitor, S);
        {From, state_change, NewServerState} ->
            NewChatServers = handle_server_state_change(From, NewServerState, S#state.chat_servers),
            NewState = S#state{chat_servers=NewChatServers},
            RouterMonitor ! {self(), state_change, NewState},
            loop(RouterMonitor, NewState);
        shutdown ->
            exit(shutdown);
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
            NewChatServers = handle_down_server(Pid, S#state.chat_servers, S#state.remote_machines),
            NewRemoteMachines = updateRemotes(S#state.remote_machines),
            NewState = S#state{chat_servers=NewChatServers, remote_machines=NewRemoteMachines},
            RouterMonitor ! {self(), state_change, NewState},
            loop(RouterMonitor, NewState);
        {From, state} ->
            From ! {self(), S#state.chat_servers, S#state.remote_machines},
            loop(RouterMonitor, S)
    end.

% -------------- Monitors the previous state ------------------

monitor_state(State = #state{}) ->
    ChatServers = create_servers(State#state.chat_servers, State#state.remote_machines),
    State#state{chat_servers=ChatServers}.

create_servers(ChatServers, RemoteMachines) ->
    create_servers(ChatServers, RemoteMachines, []).

create_servers([], _, Acc) ->
    Acc;

create_servers([{MonitorPid, Name, Server, State}|T], RemoteMachines, Acc) ->
    Ref = erlang:monitor(process, MonitorPid),
    receive
        {'DOWN', Ref, process, MonitorPid, noproc} ->
            case restore_server(Name, Server, State, RemoteMachines) of
                no_remote -> 
                    create_servers(T, RemoteMachines, Acc);
                ChatServer -> 
                    create_servers(T, RemoteMachines, [ChatServer|Acc])
            end
    after 10 ->
        create_servers(T, RemoteMachines, [{MonitorPid, Name, Server, State}|Acc])
    end.

% ------------- Get the available chat servers ----------------
get_chat_servers(ChatServers) ->
    get_chat_servers(ChatServers, []).

get_chat_servers([], Acc) -> Acc;

get_chat_servers([{_, ServerName, RemoteMachine, _}|T], Acc) ->
    get_chat_servers(T, [{ServerName, RemoteMachine}|Acc]).

% -------------------- Handle Down Server ----------------------

handle_down_server(Pid, ChatServers, RemoteMachines) ->
    handle_down_server(Pid, ChatServers, RemoteMachines, []).

handle_down_server(_, [], _, Acc) ->
    Acc;

handle_down_server(Pid, [{Pid,Name,RemoteMachine, State}|T], RemoteMachines, Acc) ->
    case restore_server(Name, State, RemoteMachine, RemoteMachines) of
        no_remote -> 
            T ++ Acc;
        ChatServer -> 
            [ChatServer|T] ++ Acc
    end;

handle_down_server(Pid, [H|T], RemoteMachines, Acc) ->
    handle_down_server(Pid, T, RemoteMachines, [H|Acc]).

restore_server(Name, State, RemoteMachine, []) ->
    case ping_remote(RemoteMachine) of
        pong ->
            {NewMonitorPid, _} = server_monitor:start_monitor({?MODULE, node()}, RemoteMachine, Name, State),
            {NewMonitorPid, Name, RemoteMachine, State};
        pang ->
            no_remote
    end;

restore_server(Name, State, RemoteMachine, [H|T]) ->
    case ping_remote(RemoteMachine) of
        pong ->
            {NewMonitorPid, _} = server_monitor:start_monitor({?MODULE, node()}, RemoteMachine, Name, State),
            {NewMonitorPid, Name, RemoteMachine, State};
        pang ->
            restore_server(Name, State, H, T)
    end.

% ---------------- Handle Server State Change ------------------
handle_server_state_change(MonitorPid, State, ChatServers) ->
    handle_server_state_change(MonitorPid, State, ChatServers, []).

handle_server_state_change(_, _, [], Acc) ->
    Acc;

handle_server_state_change(MonitorPid, State, [{MonitorPid,Name,RemoteMachine,_OldState}|T], Acc) ->
    [{MonitorPid,Name,RemoteMachine,State}|T] ++ Acc;

handle_server_state_change(MonitorPid, State, [H|T], Acc) ->
    handle_server_state_change(MonitorPid, State, T, [H|Acc]).

% ------------- Handle creation of a new server ----------------
handle_new_server(From, MsgRef, S, RemoteMachine, Name) ->
    case ping_remote(RemoteMachine) of
        pong ->
            NewRemoteMachines = add_remote_machine(RemoteMachine, S#state.remote_machines),
            case server_exists(Name, S#state.chat_servers) of
                true ->
                    From ! {MsgRef, server_already_exists},
                    S#state{remote_machines=NewRemoteMachines};
                false ->
                    {NewMonitorPid, _} = server_monitor:start_monitor({?MODULE, node()}, RemoteMachine, Name, []),
                    NewChatServers = [{NewMonitorPid, Name, RemoteMachine, []}|S#state.chat_servers],
                    From ! {MsgRef, created_server},
                    S#state{chat_servers=NewChatServers, remote_machines=NewRemoteMachines}
            end;
        pang ->
            From ! {MsgRef, remote_not_available},
            S
    end.

% ----------- Add remote machine if it isn't known already --------------
add_remote_machine(RemoteMachine, RemoteMachines) ->
    add_remote_machine(RemoteMachine, RemoteMachines, []).

add_remote_machine(RemoteMachine, [], Acc) ->
    [RemoteMachine|Acc];

add_remote_machine(RemoteMachine, [RemoteMachine|T], Acc) ->
    [RemoteMachine|T] ++ Acc;

add_remote_machine(RemoteMachine, [H|T], Acc) ->
    add_remote_machine(RemoteMachine, T, [H|Acc]).

updateRemotes(RemoteMachines) ->
    updateRemotes(RemoteMachines, []).

updateRemotes([], Acc) ->
    Acc;

updateRemotes([H|T], Acc) ->
    case ping_remote(H) of
        pong -> 
            updateRemotes(T, [H|Acc]);
        pang -> 
            updateRemotes(T, Acc)
    end.

% ------- Check if the server with a given name already exists --------
server_exists(_, []) ->
    false;

server_exists(Name, [{_, Name, _, _, _}|_]) ->
    true;

server_exists(Name, [_|T]) ->
    server_exists(Name, T).
