-module(my_calc).
-export([add/2]).
add(A, 0) -> erlang:error(division_by_zero); % Intentionally crash if B is 0
add(A, B) -> A + B.
