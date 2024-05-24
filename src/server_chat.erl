-module(server_chat).
-export([init/2, start/3, start_link/3, terminate/1]).

start(Name, Monitor, StartingState) ->
    register(Name, Pid=spawn(?MODULE, init, [Monitor, StartingState])),
    Pid.
 
start_link(Name, Monitor, StartingState) ->
    register(Name, Pid=spawn_link(?MODULE, init, [Monitor, StartingState])),
    Pid.
 
terminate(Name) ->
    Name ! shutdown.

init(Monitor, PrevState) ->
    monitor_state(PrevState),
    loop(Monitor, PrevState).

loop(Monitor, S) ->
    receive
        {From, MsgRef, join_server} ->
            erlang:monitor(process, From),
            NewClients = case exists(From, S) of
                true ->
                    From ! {MsgRef, already_in_server},
                    S;
                false ->
                    Tmp = [From | S],
                    From ! {MsgRef, ok},
                    Tmp
                end,
            Monitor ! {self(), state_change, NewClients},
            loop(Monitor, NewClients);
        {From, MsgRef, {send, Msg}} ->
            case exists(From, S) of
                true ->
                    send_to_clients(From, Msg, S),
                    From ! {MsgRef, ok},
                    loop(Monitor, S);
                false ->
                    From ! {MsgRef, not_in_server},
                    loop(Monitor, S)
            end;
        {From, MsgRef, leave_server} ->
            NewClients = removeClient(From, S),
            From ! {MsgRef, ok},
            Monitor ! {self(), state_change, NewClients},
            loop(Monitor, NewClients);
        shutdown ->
            exit(shutdown);
        {'DOWN', _Ref, process, Pid, _Reason} ->
            NewClients = removeClient(Pid, S),
            Monitor ! {self(), state_change, NewClients},
            loop(Monitor, NewClients)
    end.

monitor_state([]) ->
    finished;

monitor_state([H|T]) ->
    erlang:monitor(process, H),
    monitor_state(T).

exists(_, []) ->
    false;

exists(E, [E|_]) ->
    true;

exists(E, [_|T]) ->
    exists(E, T).

send_to_clients(_, _, []) ->
    done;

send_to_clients(From, Msg, [Client|T]) ->
    Client ! {self(), From, Msg},
    send_to_clients(From, Msg, T).

removeClient(Client, Clients) ->
    removeClient(Client, Clients, []).

removeClient(_, [], UpdatedClients) ->
    UpdatedClients;

removeClient(Client, [Client|T], UpdatedClients) ->
    T ++ UpdatedClients;

removeClient(Client, [H|T], UpdatedClients) ->
    removeClient(Client, T, [H|UpdatedClients]).
