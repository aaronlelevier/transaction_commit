%% Source: https://www.erlang.org/doc/design_principles/statem.html#example
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start_link/1]).
-export([button/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

%% API

start_link(Code) ->
  gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).

button(Button) ->
  gen_statem:cast(?NAME, {button,Button}).

%% gen_statem

init(Code) ->
  do_lock(),
  Data = #{code => Code, length => length(Code), buttons => []},
  {ok, locked, Data}.

terminate(_Reason, State, _Data) ->
  State =/= locked andalso do_lock(),
  ok.

callback_mode() ->
  state_functions.

%% States
%% https://www.erlang.org/doc/man/gen_statem.html#Module:StateName-3
%% Module:StateName(EventType, EventContent, Data) -> StateFunctionResult

locked(
  cast, {button,Button},
  #{code := Code, length := Length, buttons := Buttons} = Data) ->
  NewButtons =
    if
      length(Buttons) < Length ->
        Buttons;
      true ->
        tl(Buttons)
    end ++ [Button],
  if
    NewButtons =:= Code -> % Correct
      do_unlock(),
      {next_state, open, Data#{buttons := []},
        [{state_timeout,5000,lock}]}; % 5 sec ~ Time in milliseconds
    true -> % Incomplete | Incorrect
      io:format("~p locked; buttons: ~p~n", [self(), NewButtons]),
      {next_state, locked, Data#{buttons := NewButtons}}
  end.

open(state_timeout, lock,  Data) ->
  do_lock(),
  {next_state, locked, Data};
open(cast, {button,_}, Data) ->
  {next_state, open, Data}.

%% Helpers

do_lock() ->
  io:format("Lock~n", []).
do_unlock() ->
  io:format("Unlock~n", []).
