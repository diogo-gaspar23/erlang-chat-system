# TheyChat!

## Behaviours

### Router Node

#### Router

- Starts and monitors Servers
- Sends available Servers
- Router restores Servers when they go down
    - It first tries the Node where the Server was
    - If Node is not available, it starts the Server in another Node (includes starting the server in its own node if needed)
- Router stores state backups for the Servers
- Restarts Router Monitor in case of failure

#### Router Monitor

- Starts and creates link to Router
- Stores state backup for Router
- Restarts Router in case of failure

### Server Node

#### Server Chat

- Allows clients to join server
- Allows clients to leave server
- Monitors clients and removes clients when they go down

#### Server Monitor

- Starts and links to Server Chat
- Stores state backup for linked Server Chat
- Restarts linked Server Chat when it goes down

### Client Node

#### Client

- Interacts with the router to maintain a list of the available servers
- Joins a server (Can only join one server at a time)
- Leaves a server (Will leave current server to enter another one)
- Sends a message to the server

## How to run it

There is a Emakefile for compiling the source code files.
First create a new directory called ebin inside the project root directory:

```
mkdir ebin
```

Then, use the following command to compile the files.

```
erl -make
```

This will compile the files and keep them inside the ebin directory. To run the compiled code use the following command:

```
erl -pa ebin/ -sname node_name
```

To start a new Router Node use the following command in the erlang shell:

```erlang
router_monitor:start().
```

In order to create a server, send the following message to the Router:

```erlang
router ! {self(), make_ref(), {add_server, server_topic, 'mynode@MYPC'}}.
```

To start a new Client Node use the following command in the erlang shell:

```erlang
client:start(my_name, 'mynode@MYPC').
```
