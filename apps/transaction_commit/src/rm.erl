%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rm).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stutter/1,
  prepare/1,
  commit/1,
  abort/1
]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%%%%%% Macros %%%%%%
%% general
-define(SERVER, ?MODULE).

%% commands - to take an action
-define(PREPARE, 'PREPARE').
-define(COMMIT, 'COMMIT').
-define(ABORT, 'ABORT').

%% states - that can be assumed by the rm
-define(WORKING, 'WORKING').
-define(PREPARED, 'PREPARED').
-define(COMMITTED, 'COMMITTED').
-define(ABORTED, 'ABORTED').

%% debug
-define(GET_STATE, 'GET_STATE').

%%
-define(ACTION_MAP, #{
  ?PREPARE => ?PREPARED,
  ?COMMIT => ?COMMITTED,
  ?ABORT => ?ABORTED
}).

-define(ALLOWED_MAP, #{
  ?PREPARE => [?PREPARED, ?WORKING],
  ?COMMIT => [?COMMITTED, ?PREPARED],
  ?ABORT => [?ABORTED, ?WORKING, ?PREPARED]
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, ?WORKING, []).

%% commands
prepare(Pid) ->
  gen_server:call(Pid, ?PREPARE).

commit(Pid) ->
  gen_server:call(Pid, ?COMMIT).

abort(Pid) ->
  gen_server:call(Pid, ?ABORT).

stutter(Pid) ->
  gen_server:call(Pid, ?GET_STATE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init(State) ->
  {ok, State}.

handle_call(?GET_STATE, _From, State) ->
  {reply, {ok, State}, State};
handle_call(Request, _From, State) when is_map_key(Request, ?ACTION_MAP) ->
  {ok, NewState} = action(Request, State),
  {reply, {ok, NewState}, NewState};
handle_call(Request, _From, State) ->
  io:format("call default | Request:~s State:~s~n", [Request, State]),
  {reply, ok, State}.

handle_cast(Request, State) ->
  io:format("cast default | Request:~s State:~s~n", [Request, State]),
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

action(Action, State) ->
  Allowed = maps:get(Action, ?ALLOWED_MAP),
  true = lists:member(State, Allowed),
  NewState = maps:get(Action, ?ACTION_MAP),
  {ok, NewState}.
