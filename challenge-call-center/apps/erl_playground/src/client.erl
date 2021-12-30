-module(client).

-export([run/0]).

-define(OP_TIMEOUT, application:get_env(erl_playground, maxSeconds)).
-define(OP_MAXREQ, application:get_env(erl_playground, maxMsg)).

-record(functionality, {name, handler}).

functionalities() ->
    [
        #functionality{name = "Request user ID", handler = fun handle_userID/0},
        #functionality{name = "Joke of the day", handler = fun handle_joke/0},
        #functionality{name = "Ask for an operator", handler = fun handle_operator_req/0},
        #functionality{name = "Close the call", handler = fun () -> quit end}
    ].

-spec functionality_selection(Funcnumber :: integer) -> ok.
functionality_selection(Funcnumber) ->
    case io:fread("Select the desired functionality > ", "~d") of
        {ok, [Selection]} when Selection >= 1 andalso Selection =< Funcnumber -> 
            Selection;
        _Else ->
            io:format("Option not available.~n"), functionality_selection(Funcnumber)
    end.

main_menu(Functionalities) ->
    io:format("~n"
                "-------------~n"
                "* MAIN MENU *~n"
                "-------------~n"),
    io:format(build_menu_msg(Functionalities)),
    Number_functionalities = length(Functionalities),
    I = functionality_selection(Number_functionalities),
    Selection = lists:nth(I, Functionalities),
    (Selection#functionality.handler)().

build_menu_msg(Functionalities) -> build_menu_msg("", 1, Functionalities).
build_menu_msg(Msg, _, []) -> Msg;
build_menu_msg(Msg, N, [#functionality{name = Name} | Functionalities]) ->
    New_line = io_lib:format("~s~b. ~p~n", [Msg, N, Name]),
    build_menu_msg(New_line, N+1, Functionalities).

run() ->
    Flusher = spawn(fun flush/0),
    sockclient:connect(Flusher),
    io:format("-------------------------------~n"
              "* Welcome to CallCenter v1.0! *~n"
              "-------------------------------~n"),
    save_username(),
    loop(functionalities()).

loop(Functionalities) ->
    timer:sleep(100),
    case main_menu(Functionalities) of
        quit -> ok;
        _ -> loop(Functionalities)
    end.

save_username() ->
    Username = ask("Please insert your username: "),
    sockclient:send_create_session(Username).

ask(Prompt) ->
    case io:get_line(Prompt) of
        eof ->
            io:format("--------------------------~n"
                      "* ERROR: Invalid input.  *~n"
                      "--------------------------~n"),
            ask(Prompt);
        {error, Desc} ->
            io:format("--------------------------~n"
                      "* ERROR:                 *~n"
                      "* ~s *~n"
                      "--------------------------~n", [Desc]),
            ask(Prompt);
        Input ->
            case string:trim(Input) of
                "" -> ask(Prompt);
                _ -> string:trim(Input)
            end
    end.

handle_userID() ->
    sockclient:send_user_id_req().

handle_joke() ->
    sockclient:send_joke_req().

handle_operator_req() ->
    sockclient:send_operator_req(),
    io:format("Write 'bye' to quit chat.~n"),
    Operator = spawn(fun check_timeout/0),
    operator_chat_loop(0, Operator).

operator_chat_loop(Interaction, Operator) ->
    {ok, MaxIterations} = ?OP_MAXREQ,
    if
        Interaction =:= MaxIterations ->
            sockclient:send_operator_quit_req(),
            Operator ! "max",
            ok;
        true ->
            case ask("> ") of
                "bye" ->
                    ok;
                Msg ->
                    Requests = ask_to_operator(Msg, Interaction, Operator),
                    if 
                        Requests < 0 ->
                            sockclient:send_operator_msg_req(Msg, Interaction),
                            sockclient:send_operator_quit_req();
                            ok;
                        true ->
                            operator_chat_loop(Requests, Operator)
                    end
            end
    end.

flush() ->
    receive
        Message ->
            io:format(Message),
            flush()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Operator section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ask_to_operator(Question, Num_interaction, Operator) -> 
    {ok, Number} = ?OP_MAXREQ,
    case Num_interaction < Number of
        true ->
            Answer = process_question(Question),
            Interaction = Num_interaction + 1,
            sockclient:send_operator_msg_req(Answer, Interaction),
            Operator ! "ok",
            Interaction;
        false ->
            %Msg = "Maximum",
            Operator ! "max",
            sockclient:send_operator_quit_req()
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

check_timeout() ->
    {ok, Number} = ?OP_TIMEOUT,
    Seconds = Number * 1000,

    receive
        Msg ->
            case Msg of
                "ok" ->
                    check_timeout();
                _ ->
                    quit
            end
    after
        Seconds ->
            sockclient:send_operator_quit_req(),
            quit
    end.