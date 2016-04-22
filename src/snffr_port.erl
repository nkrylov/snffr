%% An interface with C applcation
%% that uses libpcap to capture IP packets

-module(snffr_port).

-behaviour(gen_server).

-export([start/2,
         start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([attach/0, 
         detach/0,
         get_packet/0,
         stop/0]).

-record(state, {port}).

%% API

start(PortName, PollTmo) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [PortName, PollTmo], []).

start_link(PortName, PollTmo) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [PortName, PollTmo], []).


attach() ->
  gen_server:call(?MODULE, attach).

detach() ->
  gen_server:call(?MODULE, detach).

get_packet() ->
  gen_server:call(?MODULE, get_packet).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Helper functions
log(Msg) -> io:format(Msg ++ "~n", []).
log(Msg, Args) -> io:format(Msg ++ "~n", Args).

send_to_port(Port, Data) -> port_command(Port, term_to_binary(Data)).

%% GS callbacks
init([PortName, PollTmo]) -> 
  process_flag(trap_exit, true),
  case code:priv_dir(snffr) of
    {error, Reason} = Ret -> log("priv_dir failed with: ~p", [Reason]), Ret;
    Dir -> 
      ExecName = filename:join([Dir, "bin", PortName]), 
      Port = open_port({spawn_executable, ExecName},
                       [binary, {packet, 2}, exit_status, {args, [integer_to_list(PollTmo)]}]),
      {ok, #state{port = Port}}
  end.

handle_call(Cmd, From, #state{port = Port} = State) when is_atom(Cmd) ->
  send_to_port(Port, {From, Cmd}),
  {noreply, State}; % Will reply when the port send something back

handle_call(_, _, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
  {To, Reply} = binary_to_term(Data),
  gen_server:reply(To, Reply), %% synch
  {noreply, State};
 
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) when Status > 128 ->
  log("port terminated with signal: ~p~n", [Status-128]),
  {stop, {port_terminated, Status}, State};

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State)->
  log("Port terminated with status: ~p~n", [Status]),
  {stop, {port_terminated, Status}, State};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
  {stop, {port_terminated, Reason}, State};

handle_info(_, State) -> {noreply, State}.

terminate({port_terminated, _}, _State) -> ok;
terminate(_Reason, #state{port = Port}) -> port_close(Port).

code_change(_OldVsn, State, _Extra) -> {ok, State}.





