%% cluedo_game.erl
%% This module implements a simple distributed game for teaching tracing.
%% It creates 9 nodes (each representing a room) on localhost.
%% One node (here 'room9@localhost') is safe so that the victim cannot be killed there.
%% There is one victim and a number of killer actors. Actors move by spawning on a new node and exiting.
%% To run, ensure all nodes are started (with the same cookie) and the code is loaded on each.

-module(cluedo_game).
-export([start_game/1, actor_loop/2, victim_loop/1, move_to/2, random_room/0]).

-define(ROOMS, ['room1@localhost', 'room2@localhost', 'room3@localhost',
                'room4@localhost', 'room5@localhost', 'room6@localhost',
                'room7@localhost', 'room8@localhost', 'room9@localhost']).
-define(SAFE_ROOM, 'room9@localhost').

%% start_game(TotalActors)
%% TotalActors must be at least 2 (one victim plus at least one killer).
start_game(TotalActors) when TotalActors >= 2 ->
    io:format("Starting game with ~p actors (1 victim, ~p killers)~n",
              [TotalActors, TotalActors - 1]),
    %% Seed the random generator (you may wish to use a more robust seeding method)
    random:seed(erlang:monotonic_time(), erlang:unique_integer(), erlang:system_time()),
    Nodes = ?ROOMS,
    %% Choose a killable room (i.e. not the safe room) for the victim.
    KillableRooms = lists:delete(?SAFE_ROOM, Nodes),
    VictimRoom = lists:nth(random:uniform(length(KillableRooms)), KillableRooms),
    %% Spawn the victim process on VictimRoom.
    spawn(VictimRoom, ?MODULE, victim_loop, [VictimRoom]),
    timer:sleep(100),  %% brief pause to allow victim to register globally
    spawn_killers(TotalActors - 1, Nodes).

%% spawn_killers(Count, Nodes)
%% Spawns Count killer actor processes on random nodes.
spawn_killers(0, _Nodes) ->
    ok;
spawn_killers(Count, Nodes) ->
    ActorId = Count,
    StartRoom = random_room(),
    spawn(StartRoom, ?MODULE, actor_loop, [ActorId, StartRoom]),
    spawn_killers(Count - 1, Nodes).

%% actor_loop(ActorId, CurrentRoom)
%% An actor checks if the victim is on the same node and in a killable room.
%% If so, it sends a kill message. Then it waits a moment, picks a new room, and "moves".
actor_loop(ActorId, CurrentRoom) ->
    VictimPid = global:whereis_name(victim),
    io:format("Actor ~p is in room ~p on node ~p~n", [ActorId, CurrentRoom, node()]),
    case VictimPid of
         undefined ->
             io:format("Victim not present. Actor ~p resting...~n", [ActorId]);
         _ ->
             %% Check: if the victim is in a killable room and on the same node,
             %% then attempt the "murder".
             if node(VictimPid) =:= ?SAFE_ROOM ->
                     io:format("Victim is in the safe room (~p). Actor ~p cannot kill.~n",
                               [node(VictimPid), ActorId]);
                node(VictimPid) =:= node() ->
                     io:format("Actor ~p found victim in killable room ~p. Initiating murder!~n",
                               [ActorId, node()]),
                     VictimPid ! {kill, ActorId};
                true ->
                     ok
             end
    end,
    timer:sleep(1000),
    NewRoom = random_room(),
    move_to(ActorId, NewRoom).

%% move_to(ActorId, NewRoom)
%% Simulates an actor moving by spawning a new process on NewRoom and exiting the current one.
move_to(ActorId, NewRoom) ->
    io:format("Actor ~p moving from node ~p to node ~p~n", [ActorId, node(), NewRoom]),
    spawn(NewRoom, ?MODULE, actor_loop, [ActorId, NewRoom]),
    exit(normal).

%% victim_loop(CurrentRoom)
%% The victim registers itself globally so that actors can locate it.
%% It waits to receive a kill message; if none arrives within 5 seconds, it "moves" to a new room.
victim_loop(CurrentRoom) ->
    %% Register globally (ensure a unique cookie is shared among nodes)
    global:register_name(victim, self()),
    io:format("Victim is in room ~p on node ~p~n", [CurrentRoom, node()]),
    receive
        {kill, KillerId} ->
            io:format("Victim killed by actor ~p in room ~p!~n", [KillerId, node()]),
            exit(normal)
    after 5000 ->
         %% After 5 seconds the victim moves. Note that the new room could be safe.
         NewRoom = random_room(),
         io:format("Victim moving from room ~p to room ~p~n", [CurrentRoom, NewRoom]),
         spawn(NewRoom, ?MODULE, victim_loop, [NewRoom]),
         exit(normal)
    end.

%% random_room/0 selects a random node from the defined ROOMS.
random_room() ->
    Nodes = ?ROOMS,
    lists:nth(random:uniform(length(Nodes)), Nodes).