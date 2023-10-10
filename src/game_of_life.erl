-module(game_of_life).

-export([evolve/2]).

-include_lib("eunit/include/eunit.hrl").

alive_neighbours(Neighbours) ->
    lists:foldl(
      fun(alive, Acc) -> Acc+1;
         (_, Acc) -> Acc
      end,
      0,
      Neighbours).

evolve(State, Neighbours) ->
    next_generation(State, alive_neighbours(Neighbours)).

next_generation(alive, 2) -> alive;
next_generation(alive, 3) -> alive;
next_generation(dead, 3) -> alive;
next_generation(_, _) -> dead.

dead_test() ->
    ?assertEqual(dead, evolve(dead, [])),
    ?assertEqual(dead, evolve(dead, [dead])),
    ?assertEqual(dead, evolve(dead, [dead, dead, dead])),
    ?assertEqual(alive, evolve(dead, [alive, alive, alive])),
    ?assertEqual(alive, evolve(dead, [alive, alive, alive, dead])),
    ?assertEqual(alive, evolve(dead, [dead, alive, alive, alive])).

alive_test() ->
    ?assertEqual(dead, evolve(alive, [])),
    ?assertEqual(alive, evolve(alive, [alive, alive])),
    ?assertEqual(alive, evolve(alive, [alive, dead, alive, alive])).
