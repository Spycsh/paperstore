%%%-------------------------------------------------------------------
%% @doc paperstore top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(paperstore_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    paperstore_db:load_queries(),
    {ok, {{one_for_all, 0, 1}, []}}.
