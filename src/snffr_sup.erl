%%%-------------------------------------------------------------------
%% @doc snffr top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(snffr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD_SPEC(Module, Args), {Module, {Module, start_link, Args}, permanent, 1000, worker, [Module]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, AppName} = application:get_env(app_name),
    {ok, PollTmo} = application:get_env(poll_tmo), %libpcap timeout for pcap_next()
    Modules = [
      ?CHILD_SPEC(snffr_port, [AppName, PollTmo]),
      ?CHILD_SPEC(snffr_packet_mgr, [])
    ],
    {ok, { {one_for_all, 0, 1}, Modules} }.

%%====================================================================
%% Internal functions
%%====================================================================
