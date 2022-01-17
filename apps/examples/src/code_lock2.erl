%% Source: https://www.erlang.org/doc/design_principles/statem.html#example
-module(code_lock2).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start_link/1]).
-export([button/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([
  handle_event/4
]).

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
  handle_event_function.

%% States: event callback mode
%% Module:handle_event(EventType, EventContent, State, Data)
handle_event(cast, {button,Button}, State, #{code := Code} = Data) ->
  case State of
    locked ->
      #{length := Length, buttons := Buttons} = Data,
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
            [{state_timeout,5000,lock}]}; % Time in milliseconds
        true -> % Incomplete | Incorrect
          {keep_state, Data#{buttons := NewButtons}}
      end;
    open ->
      keep_state_and_data
  end;
handle_event(state_timeout, lock, open, Data) ->
  do_lock(),
  {next_state, locked, Data};
handle_event(
  {call,From}, code_length, _State, #{code := Code} = Data) ->
  {keep_state, Data,
    [{reply,From,length(Code)}]}.

%% Helpers

do_lock() ->
  io:format("Lock~n", []).
do_unlock() ->
  io:format("Unlock~n", []).
