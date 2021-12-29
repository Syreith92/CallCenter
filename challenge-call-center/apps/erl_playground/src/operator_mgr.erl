-module(operator_mgr).

-export([start/0, ask/2]).

-define(OP_TIMEOUT, application:get_env(erl_playground, maxMsg)).
-define(OP_MAXREQ, application:get_env(erl_playground, maxMsg)).

-record(state, {received_questions = 0}).

start() ->
    Timeout = spawn(fun check_timeout/0),
    set_timer(Timeout).
    
ask(Question, Num_interaction) -> 
    case Num_interaction < ?OP_MAXREQ of
        true ->
            Answer = process_question(Question),
            Interaction = Num_interaction + 1,
            sockclient:send_operator_msg_req(Answer, Interaction),
            Interaction;
        false ->
            Msg = "Maximum",
            sockclient:send_operator_msg_req(Msg, Num_interaction)
    end.

process_question(Question) ->
    {Number, Rest} = string:to_integer(Question),
    case Number =:= error of
        true ->
            Msg = Question;
        false ->
            Even = (Number rem 2),
            case Even =:= 0 of
                true ->
                    Msg = "The number is even!";
                false ->
                    Msg = "The number is odd!"
            end
    end.

set_timer(Timeout) ->
    lager:warning("Sono in set_timer()"),
    {Number, Rest} = string:to_integer(?OP_TIMEOUT),
    case Number of
        error ->
            Seconds = 10000;
        _ ->
            Seconds = Number * 1000
    end,

    lager:warning("Seconds ~p", [Seconds]),    

    erlang:send_after(Seconds, Timeout, "Timeout").

check_timeout() ->
    receive
        Msg ->
            sockclient:send_operator_msg_req(Msg, 0),
            sockclient:send_operator_quit_req();
        _ ->
            check_timeout()
    end.





    %case Msg =:= "Timeout" of
    %    timeout ->
     %       sockclient:send_operator_msg_req(Msg),
      %      sockclient:send_operator_quit_req();
       % _ ->
        %    timeout()
