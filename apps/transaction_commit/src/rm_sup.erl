%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rm_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
    #{
      strategy => one_for_one,
      intensity => 5,
      period => 30
    },
    [
      #{
        id => N,
        start => {rm, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [rm]
      } || N <- lists:seq(1, 3)
    ]
  }
  }.
