-module(e_time_slicer_dynamic_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(test(T), {setup, fun start/0, fun stop/1, T}).

start() -> ok.
stop(_) -> ok.

test({input, Input, expected_result, ExpectedResult}) ->
  fun() ->
    {ok, Result} = erlang:apply(e_time_slicer, slice, Input),

    ?assertEqual(ExpectedResult, Result)
  end.

all_tests_test_() ->
  TestCases = [
%%    {
%%      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
%%      %% 1609459200 - 01 Jan 2021 00:00:00 GMT
%%      input, [1577836800, 1609459200, [{dynamic, true}, {max_scale, weeks}]],
%%      expected_result, [
%%        {weeks, 52},
%%        {days, 2},
%%        {slices, [
%%          [{type, weeks}, {from, 1}, {to, 2}],
%%          [{type, days}, {from, 1}, {to, 2}]
%%        ]}
%%      ]
%%    }
  ],

  [
    ?test(fun(_) -> {timeout, 1000, test(TestCase)} end) ||
    TestCase <- TestCases
  ].
