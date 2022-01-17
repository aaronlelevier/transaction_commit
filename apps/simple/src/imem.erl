%% Erlang implementation of https://github.com/tlaplus/Examples/blob/master/specifications/SpecifyingSystems/CachingMemory/InternalMemory.tla
-module(imem).
-behaviour(gen_statem).

-export([start_link/0]).
-export([
  req/2,
  do/1,
  rsp/1
]).
-export([init/1,callback_mode/0,terminate/3]).
-export([
  handle_event/4
]).

%% Records
-record(mreq, {op, adr, val}).

%% TODO: move to .hrl file ?
-define(TABLE, mem).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_statem:start_link(?MODULE, [], []).

%% write req
req(Pid, {Adr, Val} = _Req) ->
  WrReq = #mreq{op="Wr", adr=Adr, val=Val},
  gen_statem:cast(Pid, {req, WrReq});
%% read req
req(Pid, {Adr} = _Req) ->
  RdReq = #mreq{op="Rd", adr=Adr},
  gen_statem:cast(Pid, {req, RdReq}).

do(Pid) ->
  gen_statem:cast(Pid, do).

rsp(Pid) ->
  gen_statem:cast(Pid, rsp).

%%%===================================================================
%%% gen_statem
%%%===================================================================

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
  io:format("Send p:~p req:~p~n", [self(), Req]),
  NewState = "busy",
  NewData = Req,
  io:format("NewState:~p NewData:~p~n", [NewState, NewData]),
  {next_state, NewState, NewData};

handle_event(cast, do, "busy" = _State, _Data = #mreq{op=Op, adr=Adr, val=Val}) ->
  case Op of
    "Wr" ->
      ok = dets:insert(?TABLE, {Adr, Val}),
      NewData = undefined;
    "Rd" ->
      [{Adr, NewVal}] = dets:lookup(?TABLE, Adr),
      NewData = NewVal
  end,
  NewState = "done",
  io:format("NewState:~p NewData:~p~n", [NewState, NewData]),
  {next_state, NewState, NewData};

handle_event(cast, rsp, "done" = _State, Data) ->
  io:format("Reply p:~p buf:~p~n", [self(), Data]),
  NewState = "rdy",
  io:format("NewState:~p NewData:~p~n", [NewState, Data]),
  {next_state, NewState, Data};

%% unhandled event catch-all for debugging
handle_event(EventType, EventContent, State, Data) ->
  io:format(
    "EventType:~p, EventContent:~p, State:~p, Data:~p~n",
    [EventType, EventContent, State, Data]),
  % stuttering step
  % {next_state, State, Data}.
  keep_state_and_data.
