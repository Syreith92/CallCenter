-module(jokes).

-export([init/0, get_joke_for_today/1]).

load_jokes(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Binary.
    %[binary_to_list(Bin) || Bin <- binary:split(Binary, <<"\n">>, [global]), Bin =/= << >>].

init() ->
    {ok, Jokes_file} = application:get_env(erl_playground, jokes_file),
    Jokes = load_jokes(Jokes_file),
    T = ets:new(t, [set, named_table]),
    ets:insert(t, {jokes, Jokes}).
    %lager:info("~p ~p",[T, ets:lookup(t, jokes)]).

get_element_from_ets() ->
    Tuple = ets:lookup(t, jokes),
    Value = proplists:get_value(jokes, Tuple, undefined),
    %lager:info("Value ~p", [Value]),
    Binary = [binary_to_list(Bin) || Bin <- binary:split(Value, <<"\n">>, [global]), Bin =/= << >>],
    %lager:info("Bin ~p", [Binary]),
    Binary.

get_joke_for_today({{Y, M, D}, _}) ->
    Jokes = get_element_from_ets(),
    Elements = length(Jokes),
    Joke = lists:nth(1 + (Y+M+D) rem Elements, Jokes),
    %lager:info("Joke ~p, Y~p, M~p, D~p, Y+M+D ~p, Y+M+D rem Elements ~p", [Joke, Y, M, D, (Y+M+D), (Y+M+D) rem Elements]),
    Joke.
