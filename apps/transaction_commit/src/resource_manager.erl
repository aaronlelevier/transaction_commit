%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Erlang implementation of TLA+ TCommit Resource Manager
%%% Reference: https://github.com/tlaplus/Examples/blob/master/specifications/transaction_commit/TCommit.tla
%%% @end
%%%-------------------------------------------------------------------
-module(resource_manager).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
%% impl
-export([
  prepare/1,
  commit/1,
  abort/1
]).

%% Macros
-define(SERVER, ?MODULE).

%% State Names
-define(WORKING, "working").
-define(PREPARED, "prepared").
-define(COMMITTED, "committed").
-define(ABORTED, "aborted").

%% Requests
-define(PREPARE, "prepare").
-define(COMMIT, "commit").
-define(ABORT, "abort").

%%%===================================================================
%%% API
%%%===================================================================

%% https://www.erlang.org/doc/man/gen_server.html#start_link-3
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% Commands
prepare(Pid) -> gen_server:call(Pid, ?PREPARE).

commit(Pid) -> gen_server:call(Pid, ?COMMIT).

abort(Pid) -> gen_server:call(Pid, ?ABORT).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, "working"}.

handle_call(Request, _From, State) ->
  NextState = next(Request, State),
  {reply, {ok, #{ state => State, next_state => NextState}}, NextState}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

next(Request, State) ->
  case {Request, State} of
    {?PREPARE, ?WORKING} ->
      ?PREPARED;
    {?COMMIT, ?PREPARED} ->
      ?COMMITTED;
    {?ABORT, ?WORKING} ->
      ?ABORTED;
    {?ABORT, ?PREPARED} ->
      ?ABORTED;
    {_, _} ->
      State
  end.