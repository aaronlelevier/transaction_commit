%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rm).
-behaviour(gen_server).

%% API
-export([
  start_link/1,
  get_state/1
]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

-define(GET_STATE, 'GET_STATE').

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

get_state(Pid) ->
  gen_server:cast(Pid, ?GET_STATE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init(State) ->
  {ok, State}.

handle_call(Request, _From, State) ->
  io:format("call default | Request:~s State:~s~n", [Request, State]),
  {reply, ok, State}.

handle_cast(?GET_STATE, State) ->
  io:format("State: ~s~n", [State]),
  {noreply, State};
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
