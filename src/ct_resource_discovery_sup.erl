%%%-------------------------------------------------------------------
%% @doc ct_resource_discovery top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ct_resource_discovery_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ResourceDiscoverySpec = #{id => resource_discovery,
                           start => {ct_resource_discovery, start_link, []},
                           restart => permanent,
                           shutdown => 1000,
                           type => worker,
                           modules => [ct_resource_discovery]},
  {ok, {SupFlags, [ResourceDiscoverySpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
