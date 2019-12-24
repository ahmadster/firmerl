%%%-------------------------------------------------------------------
%% @doc htest public API
%% @end
%%%-------------------------------------------------------------------

-module(htest_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	logger:set_module_level(mc, info),
	logger:set_module_level(ws, info),
	logger:set_module_level(htest_sup, info),
	mc:start_link(),

	Router = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, htest, "index.html"}},
			{"/ws", ws, #{}},
			{"/[...]", cowboy_static, {priv_dir, htest, "ui/dist"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Router}
	}),

	htest_sup:start_link().

stop(_State) ->
	cowboy:stop_listener(http).

%% internal functions
