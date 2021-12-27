-module(userid).

-export([generate/0]).

generate() ->
    Identifier = crypto:strong_rand_bytes(8),
    base64:encode(Identifier).