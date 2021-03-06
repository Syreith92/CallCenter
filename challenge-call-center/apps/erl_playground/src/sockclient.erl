-module(sockclient).
-behaviour(gen_server).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]). -ignore_xref([{start_link, 4}]).
-export([connect/1, disconnect/0]).
-export([send_create_session/1, send_user_id_req/0, send_joke_req/0]).
-export([send_operator_req/0, send_operator_quit_req/0, send_operator_msg_req/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(),
    ref = undefined :: any()
}).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

start_link() ->
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

-spec connect(_Ref) -> ok.
connect(Ref) ->
    gen_server:call(whereis(?SERVER), {connect, Ref}),
    ok.

-spec disconnect() -> ok.
disconnect() ->
    gen_server:call(whereis(?SERVER), disconnect),
    ok.

-spec send_create_session(_) -> ok.
send_create_session(Username) ->
    Req = #req {
        type = create_session,
        create_session_data = #create_session{
            username = Username
        }        
    },

    %%CreateSession = #create_session {
    %%    username = <<"TestUser">>
    %%},
    gen_server:cast(whereis(?SERVER), {send_msg, Req}).

-spec send_user_id_req() -> ok.
send_user_id_req() ->
    Req = #req {
        type = caller_id_request    
    },
    gen_server:cast(whereis(?SERVER), {send_msg, Req}).

-spec send_joke_req() -> ok.
send_joke_req() ->
    Req = #req {
        type = jokes_request
    },
    gen_server:cast(whereis(?SERVER), {send_msg, Req}).

-spec send_operator_req() -> ok.
send_operator_req() ->
    Req = #req {
        type = operator_request
    },
    gen_server:cast(whereis(?SERVER), {send_msg, Req}).

-spec send_operator_quit_req() -> ok.
send_operator_quit_req() ->
    Req = #req {
        type = operator_quit_req
    },
    gen_server:cast(whereis(?SERVER), {send_msg, Req}).

-spec send_operator_msg_req(_Msg, _Interaction) -> ok.
send_operator_msg_req(Msg, Interaction) ->
    Req = #req {
        type = operator_msg_req,
        operator_msg = #operator_message {
            message = Msg,
            interactions = Interaction
        }
    },
    gen_server:cast(whereis(?SERVER), {send_msg, Req}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init(_ARgs) ->
    lager:info("sockclient init'ed"),
    {ok, #state{}}.

handle_cast({send_msg, Req}, #state{socket = Socket} = State)
    when Socket =/= undefined -> 
    send(Req, Socket),
    {noreply, State};
handle_cast(Message, State) ->
    _ = lager:warning("No handle_cast for ~p", [Message]),
    {noreply, State}.

handle_info({tcp_closed, _Port}, State) ->
    {noreply, State#state{socket = undefined}};
handle_info({tcp, _Port, Packet}, State) ->
    Req = utils:open_envelope(Packet),
    State = process_packet(Req, State, utils:unix_timestamp()),
    {noreply, State};
handle_info(Message, State) ->
    _ = lager:warning("No handle_info for~p", [Message]),
    {noreply, State}.

handle_call({connect, Ref}, _From, State) ->
    {ok, Host} = application:get_env(erl_playground, tcp_host),
    {ok, Port} = application:get_env(erl_playground, tcp_port),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 2}]),

    {reply, normal, State#state{socket = Socket, ref = Ref}};
handle_call(disconnect, _From, #state{socket = Socket} = State)
    when Socket =/= undefined ->
    
    gen_tcp:shutdown(Socket, read_write),

    {reply, normal, State};
handle_call(Message, _From, State) ->
    _ = lager:warning("No handle_call for ~p", [Message]),
    {reply, normal, State}.

terminate(Reason, _State) ->
    _ = lager:notice("terminate ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    lager:notice("server sent invalid packet, ignoring"),
    State;
process_packet(#req{ type = Type } = Req, #state{ref = Ref} = State, _Now)
    when Type =:= server_message ->
    #req{
        server_message_data = #server_message{
            message = Message
        }
    } = Req,

    Ref ! Message,
    State.

send(Req, Socket) ->
    Data = utils:add_envelope(Req),
    gen_tcp:send(Socket, Data).
