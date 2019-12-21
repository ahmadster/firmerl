%%%-------------------------------------------------------------------
%% @doc htest public API
%% @end
%%%-------------------------------------------------------------------

-module(htest_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  arduino_event:delete_handler(sample_event_handler),
  htest_sup:start_link().

stop(_State) ->
  ok.

%% internal functions
