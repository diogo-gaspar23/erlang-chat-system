-module(server_monitor).
-export([start/4, start/3, start_monitor/4, start_link/3, init/3]).
 
start(Router, RemoteMachine, Name, StartState) ->
    spawn(RemoteMachine, ?MODULE, init, [Router, Name, StartState]).
 
start_monitor(Router, RemoteMachine, Name, StartState) ->
    spawn_monitor(RemoteMachine, ?MODULE, init, [Router, Name, StartState]).
 
start(Router, Name, StartState) ->
    spawn(?MODULE, init, [Router, Name, StartState]).
 
start_link(Router, Name, StartState) ->
    spawn_link(?MODULE, init, [Router, Name, StartState]).

init(Router, Name, StartState) ->
    process_flag(trap_exit, true),
    Pid = server_chat:start_link(Name, self(), StartState),
    loop(Router, Name, Pid, StartState).
    
loop(Router, Name, Pid, State) ->
    receive
        {Pid, state_change, NewState} ->
            Router ! {self(), state_change, NewState},
            loop(Router, Name, Pid, NewState);
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, _Reason} ->
            io:format("Chat server went down!\nRestarting...\n"),
            NewPid = restart(Name, State),
            loop(Router, Name, NewPid, State)
    end.

restart(Name, State) ->
    server_chat:start_link(Name, self(), State).
