%%%-------------------------------------------------------------------
%%% @author ahmad
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2019 7:45 PM
%%%-------------------------------------------------------------------
-module(ht).
-author("ahmad").

%% API
-export([on/1, off/1]).

on(Port) -> arduino:digital_write(Port, [1, 1, 1, 1, 1, 1, 1, 1]), arduino:all_digital().
off(Port) -> arduino:digital_write(Port, [0, 0, 0, 0, 0, 0, 0, 0]), arduino:all_digital().

test()->
  on(0),
  timer:sleep(1000).