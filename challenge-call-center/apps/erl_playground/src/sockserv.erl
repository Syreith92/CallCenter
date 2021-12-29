-module(sockserv).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]). -ignore_xref([{start_link, 4}]).
-export([start/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Exports
%% ------------------------------------------------------------------

-export([init/4]). -ignore_xref([{init, 4}]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport,
    userID,
    username = "",
    operator
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start() ->
    {ok, Port} = application:get_env(erl_playground, tcp_port),
    {ok, MaxConnections} = application:get_env(erl_playground, max_connections),

    TcpOptions = [
        {backlog, 100}
    ],

    {ok, _} = ranch:start_listener(
        sockserv_tcp,
        ranch_tcp,
        [{port, Port},
        {num_acceptors, 100}] ++ TcpOptions,
        sockserv,
        [none]
    ),

    ranch:set_max_connections(sockserv_tcp, MaxConnections),
    lager:info("server listening on tcp port ~p", [Port]),
    ok.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Definitions
%% ------------------------------------------------------------------

init(Ref, Socket, Transport, [_ProxyProtocol]) ->
    lager:info("sockserv init'ed ~p",[Socket]),

    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),

    Opts = [{packet, 2}, {packet_size, 16384}, {active, once}, {nodelay, true}],
    _ = Transport:setopts(Socket, Opts),

    State = #state{
        socket = Socket,
        transport = Transport,
        userID = userid:generate()
    },

    gen_server:enter_loop(?MODULE, [], State).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> undefined.

handle_cast(Message, State) ->
    _ = lager:notice("unknown handle_cast ~p", [Message]),
    {noreply, State}.

handle_info({tcp, _Port, <<>>}, State) ->
    _ = lager:notice("empty handle_info state: ~p", [State]),
    {noreply, State};
handle_info({tcp, _Port, Packet}, State = #state{socket = Socket}) ->
    %%Req = utils:open_envelope(Packet),

    %%State = process_packet(Req, State, utils:unix_timestamp()),
    self() ! {packet, Packet},

    ok = inet:setopts(Socket, [{active, once}]),

    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    {stop, normal, State};
handle_info({packet, Packet}, State) ->
    Req = utils:open_envelope(Packet),
    NewState = process_packet(Req, State, utils:unix_timestamp()),
    {noreply, NewState};
handle_info({operator_removed, Ref}, #state{operator = Ref} = State) ->
    send(server_message("[server] Your operator left the chat.~n"), State),
    NewState = State#state{operator = undefined},
    {noreply, NewState};
handle_info(Message, State) ->
    _ = lager:notice("unknown handle_info ~p", [Message]),
    {noreply, State}.

handle_call(Message, _From, State) ->
    _ = lager:notice("unknown handle_call ~p", [Message]),
    {noreply, State}.

terminate(normal, _State) ->
    _ = lager:info("Goodbye!"),
    ok;
terminate(Reason, _State) ->
    _ = lager:notice("No terminate for ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    _ = lager:notice("client sent invalid packet, ignoring ~p",[State]),
    State;
process_packet(#req{ type = Type } = Req, State = #state{}, _Now) ->
    case handle_request(Type, Req, State) of
        {noreply, NewState} -> NewState;
        {Response, NewState} -> send(Response, NewState), NewState
    end.

send(Response, #state{socket = Socket, transport = Transport}) ->
    send(Response, Socket, Transport).

send(Response, Socket, Transport) ->
    Data = utils:add_envelope(Response),
    Transport:send(Socket,Data).

%% ------------------------------------------------------------------
%% Request handlers
%% ------------------------------------------------------------------
server_message(Msg) ->
    #req{
        type = server_message,
        server_message_data = #server_message {
            message = Msg
        }
    }.

handle_request(create_session, #req {create_session_data = #create_session{ username = Username}}, State) ->
    NewState = State#state{username = Username},
    {server_message(io_lib:format("Welcome ~s!~n", [Username])), NewState};
handle_request(caller_id_request, _Req, #state{userID = UID} = State) ->
    {server_message(io_lib:format("----------------~n"
                                  "* Caller ID:   *~n"
                                  "* ~s *~n"
                                  "----------------~n", [UID])),
        State};
handle_request(jokes_request, _Req, State) ->
    Joke = jokes:get_joke_for_today(calendar:now_to_datetime(os:timestamp())),
    {server_message(build_joke_msg(Joke)), State};
handle_request(operator_request, _Req, State) ->
    {server_message(build_operator_msg(State, "", 0)), State};
handle_request(operator_quit_req, _Req, State) ->
    {server_message("The operator has left the chat.~n"), State};
handle_request(operator_msg_req, #req{operator_msg = #operator_message{message = Msg, interactions = Interaction}} = Req, State) ->
    NewState = State#state{operator = Interaction},
    {server_message(build_operator_msg(State, Msg, Interaction)), NewState}.

build_joke_msg(Joke) ->
    io_lib:format("-------------------------------------------------------------~n"
                  "* Joke of the day:                                          *~n"
                  "-------------------------------------------------------------~n"
                  "~n~s~n", [Joke]).

build_operator_msg(State, Msg, Interaction) ->
    if 
        Interaction =:= 0 -> 
            io_lib:format("------------------------------------~n"
                          "  Welcome ~s!~n"
                          "* I'm your operator.               *~n"
                          "* How can I help you?              *~n"
                          "------------------------------------~n", [State#state.username]);
        false ->
            case (Msg =:= "Timeout" orelse Msg =:= "Maximum") of
                true ->
                    io_lib:format("---------------------------------~n"
                                "* Bye!                          *~n"
                                "---------------------------------~n", []);
                false ->
                    io_lib:format("---------------------------------~n"
                                  "* ~s *~n"
                                  "---------------------------------~n", [Msg])
            end
    end.
