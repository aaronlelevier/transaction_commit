%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 02. Jan 2022 3:06 PM
%%%-------------------------------------------------------------------
-module(rm_test).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

%% working -> prepared -> committed
working_prepared_committed_test() ->
  {ok, Pid} = rm:start_link(),
  ?assertEqual({ok, 'WORKING'}, rm:stutter(Pid)),
  ?assertEqual({ok, 'PREPARED'}, rm:prepare(Pid)),
  ?assertEqual({ok, 'COMMITTED'}, rm:commit(Pid)).

working_prepared_aborted_committed_test() ->
  {ok, Pid} = rm:start_link(),
  ?assertEqual({ok, 'WORKING'}, rm:stutter(Pid)),
  ?assertEqual({ok, 'PREPARED'}, rm:prepare(Pid)),
  ?assertEqual({ok, 'ABORTED'}, rm:abort(Pid)).

working_aborted_test() ->
  {ok, Pid} = rm:start_link(),
  ?assertEqual({ok, 'WORKING'}, rm:stutter(Pid)),
  ?assertEqual({ok, 'ABORTED'}, rm:abort(Pid)).

working_prepared_aborted_test() ->
  {ok, Pid} = rm:start_link(),
  ?assertEqual({ok, 'WORKING'}, rm:stutter(Pid)),
  ?assertEqual({ok, 'PREPARED'}, rm:prepare(Pid)),
  ?assertEqual({ok, 'ABORTED'}, rm:abort(Pid)).

can_stutter_on_the_same_step_test() ->
  {ok, Pid} = rm:start_link(),
  ?assertEqual({ok, 'PREPARED'}, rm:prepare(Pid)),
  ?assertEqual({ok, 'PREPARED'}, rm:stutter(Pid)).
