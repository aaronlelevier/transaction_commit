%%%-------------------------------------------------------------------
%% @doc transaction_commit public API
%% @end
%%%-------------------------------------------------------------------

-module(transaction_commit_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    transaction_commit_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
