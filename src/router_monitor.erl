-module(router_monitor).
-export([start/0, init/1]).
-record(state, {chat_servers,
                remote_machines}).

start() ->
    spawn(?MODULE, init, [#state{chat_servers=[],remote_machines=[node()]}]).

init(StartState) ->
    process_flag(trap_exit, true),
    Pid = router:start_link(self(), StartState),
    loop(Pid, StartState).
    
loop(Pid, State) ->
    receive
        {Pid, state_change, NewState} ->
            loop(Pid, NewState);
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
            restart(State)
    end.

restart(State) ->
    Pid = router:start_link(self(), State),
    loop(Pid, State).