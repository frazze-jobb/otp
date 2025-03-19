cluedo
=====

N characters in a mansion, and whenever a murder weapon, person and the victim
is in the same room, and timing is right, a murder happens, and the game is over.
You have to use tracing to find who did it.
How many ways can you crash a node/process? (behavior of the murder weapons)
Actors+victim in the game are processes, each room is a separate node.
An actor moves to a new node by sending its state and function to another node, and a new pid is created
When an actor and victim is in the same room, and the murder 
When a murder weapon is on the same node as a victim and a murderer, the crash can happen. And either the
node is resiliant and fires up a new game, or the node goes under.

Build
-----

    $ rebar3 compile
