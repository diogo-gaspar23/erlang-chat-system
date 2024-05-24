-module(client).
-export([start/2,swap_remote/2,send_msg/2,get_available_servers/1,join_server/2,leave_curr_server/1,stop_client/1]).

start(Client, RouterRemote) -> register(Client,spawn(fun() -> add_remote(RouterRemote), loop([], {}, RouterRemote) end)).

add_remote(RouterRemote) ->
    net_adm:ping(RouterRemote).

swap_remote(Client, NewRouterRemote) ->
    Client ! {swap_remote, NewRouterRemote}.

send_msg(Client,Message)->
    Client ! {send,Message}.

get_available_servers(Client) ->
    Client ! {get_available_servers}.

join_server(Client, Name) ->
    Client ! {join_server, Name}.

leave_curr_server(Client) ->
    Client ! {leave_server}.

stop_client(Client) ->
    Client ! {stop_client}.

loop(Servers, CurrServer, RouterRemote) ->
    receive
        {_Server, From, Msg} ->
            io:format("~p: ~p~n", [From, Msg]),
            loop(Servers, CurrServer, RouterRemote);
        {get_available_servers} ->
            {UpdatedCurrServer, NewServers} = get_available_servers_from_router(RouterRemote, Servers, CurrServer),
            loop(NewServers, UpdatedCurrServer, RouterRemote);
        {send, Message} when CurrServer =/= {} ->
            {UpdatedCurrServer, NewServers} = get_available_servers_from_router(RouterRemote, Servers, CurrServer),
            UpdatedCurrServer ! {self(), Ref = make_ref(), {send, Message}},
            receive
                {Ref, Reply} -> io:format("Received from server: ~p~n",[Reply])
            end,
            loop(NewServers, UpdatedCurrServer, RouterRemote);
        {send, _Message} ->
            io:format("You are not in any server."),
            loop(Servers, CurrServer, RouterRemote);
        {join_server, Name} when CurrServer =:= {} ->
            {_, NewServers} = get_available_servers_from_router(RouterRemote, Servers, CurrServer),
            Server = join_new_server(Name, NewServers),
            loop(NewServers, Server, RouterRemote);
        {join_server, Name} ->
            {UpdatedCurrServer, NewServers} = get_available_servers_from_router(RouterRemote, Servers, CurrServer),
            leave_server(UpdatedCurrServer),
            Server = join_new_server(Name, NewServers),
            loop(Servers, Server, RouterRemote);
        {leave_server} when CurrServer =/= {} ->
            {UpdatedCurrServer, NewServers} = get_available_servers_from_router(RouterRemote, Servers, CurrServer),
            leave_server(UpdatedCurrServer),
            loop(NewServers, {}, RouterRemote);
        {leave_server} ->
            io:format("You are not in a server."),
            loop(Servers, CurrServer, RouterRemote);
        {swap_remote, NewRouterRemote} ->
            loop(Servers, CurrServer, NewRouterRemote);
        {stop_client} ->
            io:format("Client exiting...")
    end.

get_available_servers_from_router(RouterRemote, Servers, CurrServer) ->
    {router, RouterRemote} ! {self(), Ref = make_ref(), get_chat_servers},
        receive
            {Ref, NewServers} ->
                print_servers(NewServers),
                UpdatedCurrServer = if
                    CurrServer =/= {} -> 
                        {CurrName, _} = CurrServer,
                        get_server(CurrName, NewServers);
                    true ->
                        CurrServer
                end,
                {UpdatedCurrServer, NewServers}
        after 1000 ->
            io:format("Router did not respond."),
            {CurrServer, Servers}
        end.

join_new_server(Name, Servers) ->
    case get_server(Name, Servers) of
        not_found -> 
            io:format("Server does not exist."),
            {};
        Server -> 
            Server ! {self(), Ref = make_ref(), join_server},
            receive
                {Ref, Reply} -> 
                    io:format("Received from server: ~p~n",[Reply]),
                    Server
            after 1000 ->
                io:format("Server did not respond."),
                {}
            end
    end.

leave_server(Server) ->
    Server ! {self(), Ref = make_ref(), leave_server},
    receive
        {Ref, Reply} -> io:format("Received from server: ~p~n",[Reply])
    after 1000 ->
            io:format("Server did not respond.")
    end.

get_server(_, []) ->
    not_found;

get_server(Name, [{Name, RemoteMachine}|_]) ->
    {Name, RemoteMachine};

get_server(Name, [_|T]) ->
    get_server(Name, T).

print_servers(Servers) ->
    print_servers(Servers, 1).

print_servers([], _) ->
    done;

print_servers([{Name, _}|T], Count) ->
    io:format("Server #~w: ~p~n", [Count, Name]),
    print_servers(T, Count + 1).
