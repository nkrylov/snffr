%% Main packet management module that allows applying input filters
%% and printing routines

-module(snffr_packet_mgr).

-behaviour(gen_server).

-export([start/0,
         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([print_hex/1]).
-record(state, {filters, iface}).

%% API

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

print_hex(Packet) when is_binary(Packet) ->
  gen_server:call(?MODULE, {print_hex, Packet}).

%% Helper funcitons

%% GS callbacks
init([]) -> 
  {ok, Iface} = snffr_port:attach(),
  io:format("Ready to capture on ~s~n", [binary_to_list(Iface)]),
  {ok, #state{filters = [], iface = Iface}}.

handle_call({print_hex, Packet}, _From, State) ->
  io:format("Implement me~n"),
  {reply, ok, State}; 

handle_call(_, _, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> 
  snffr_port:detach(),
  snffr_port:stop().

code_change(_OldVsn, State, _Extra) -> {ok, State}.





