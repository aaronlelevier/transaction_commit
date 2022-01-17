%% Source: https://www.erlang.org/doc/design_principles/statem.html#example
-module(imem).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start_link/0]).
-export([
  req/2
]).
-export([init/1,callback_mode/0,terminate/3]).
-export([
  handle_event/4
]).

%% API

start_link() ->
  gen_statem:start_link(?MODULE, [], []).

req(Pid, Req) ->
  gen_statem:cast(Pid, {req, Req}),
  {Pid, Req}.

%% gen_statem

init([]) ->
  Data = undefined,
  State = "rdy",
  {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
  ok.

callback_mode() ->
  handle_event_function.

%% States: event callback mode
%% Module:handle_event(EventType, EventContent, State, Data)
handle_event(cast, {req, Req}, "rdy" = _State, _Data) ->
  io:format("Send req:~p~n", [Req]),
  {next_state, "busy", Req};

%% unhandled event catch-all for debugging
handle_event(EventType, EventContent, State, Data) ->
  io:format(
    "EventType:~p, EventContent:~p, State:~p, Data:~p~n",
    [EventType, EventContent, State, Data]),
  % stuttering step
  % {next_state, State, Data}.
  keep_state_and_data.

%% Helpers

%%{ok, Pid} = imem:start_link().
%%imem:req(Pid, "val").
%%imem:req(Pid, "val2").
