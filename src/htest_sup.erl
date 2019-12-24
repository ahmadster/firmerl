%%%-------------------------------------------------------------------
%% @doc htest top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(htest_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-include_lib("kernel/include/logger.hrl").

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
	SupFlags = #{strategy => one_for_all,
		intensity => 0,
		period => 1},
	ChildSpecs = [],
	?LOG_INFO(#{what => "loaded applicaitons",
		applications => application:which_applications()}),
	{ok, {SupFlags, ChildSpecs}}.

%% internal functions
