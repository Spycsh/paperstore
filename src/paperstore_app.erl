%%%-------------------------------------------------------------------
%% @doc paperstore public API
%% @end
%%%-------------------------------------------------------------------

-module(paperstore_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    paperstore_sup:start_link().

stop(_State) ->
    ok.

