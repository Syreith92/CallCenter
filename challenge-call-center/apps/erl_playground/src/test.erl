-module(test).

%% Application callbacks
-export([my_test/0]).

my_test() ->
    lager:warning("pippo").