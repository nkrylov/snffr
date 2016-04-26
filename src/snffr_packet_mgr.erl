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

-export([add_printer/1, print/1]).
-record(state, {printers}).

%% API

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_printer(Printer) ->
  gen_server:call(?MODULE, {add_printer, Printer}).

print(Packet) when is_binary(Packet) ->
  gen_server:call(?MODULE, {print_packet, Packet}).

%% Helper funcitons

%% GS callbacks
init([]) -> 
  {ok, Ifaces} = snffr_port:init(),
  io:format("Welcome to snffr console. Here's what you can do:~n"),
  io:format("1. run snffr:attach(<interface>) to start listening where <interface> is one of:~n"),
  [io:format("~p~n", [binary_to_atom(Iface, utf8)]) || Iface <- Ifaces],
  io:format("2. Select a printer(s) from the list below and run snffr:add_printer(<printer>)~n"),
  io:format(" or snffr:add_printer([<p1>, ..., <pN>]) where printers are:~n"),
  N = [ Name || {Name, Arity} <- snffr_utils:module_info(exports), Name /= module_info, Arity == 1 ],
  lists:foreach(fun(P) -> io:format(" ~p~n", [P]) end, N),
  io:format("3. snffr:print(snffr:packet()). - to grab a packet and pass it through all printers you selected.~n"),
  io:format("4. type q(). to exit~n"),
  {ok, #state{printers = []}}.

handle_call({add_printer, Printer}, _From, #state{printers = Printers} = State) ->
  {reply, ok, State#state{printers = [Printer | Printers]}};

handle_call({print_packet, Packet}, _From, #state{ printers = Printers} = State) ->
  [ Printer(Packet) || Printer <- Printers ],
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





