-module(router_monitor).
-export([start/0, restart_monitor/2, init/1, init/2]).
-record(state, {chat_servers,
                remote_machines}).

start() ->
    register(?MODULE, spawn(?MODULE, init, [#state{chat_servers=[],remote_machines=[node()]}])).

init(StartState) ->
    process_flag(trap_exit, true),
    Pid = router:start_link(self(), StartState),
    loop(Pid, StartState).

restart_monitor(Router, State) ->
    register(?MODULE, Pid = spawn(?MODULE, init, [Router, State])),
    Pid.

init(Router, StartState) ->
    process_flag(trap_exit, true),
    erlang:link(Router),
    loop(Router, StartState).
    
loop(Pid, State) ->
    receive
        {Pid, state_change, NewState} ->
            loop(Pid, NewState);
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, _Reason} ->
            io:format("Router went down.\nRestarting...\n"),
            NewPid = restart(State),
            loop(NewPid, State)
    end.

restart(State) ->
    router:start_link(self(), State).