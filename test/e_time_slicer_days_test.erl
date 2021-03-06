-module(e_time_slicer_days_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("include/e_time_slicer.hrl").

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
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      input, [1577836800, 1577836801, #options{scale = days, is_dynamic = false, is_zero_based = false}],
      expected_result, [
        {type, days},
        {count, 0},
        {from, 1577836800},
        {to, undefined},
        {remainder, 1},
        {slices, []}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836861 - 01 Jan 2020 00:01:01 GMT
      input, [1577836800, 1577836861, #options{scale = days, is_dynamic = false, is_zero_based = false}],
      expected_result, [
        {type, days},
        {count, 0},
        {from, 1577836800},
        {to, undefined},
        {remainder, 61},
        {slices, []}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577840461 - 01 Jan 2020 01:01:01 GMT
      input, [1577836800, 1577840461, #options{scale = days, is_dynamic = false, is_zero_based = false}],
        expected_result, [
        {type, days},
        {count, 0},
        {from, 1577836800},
        {to, undefined},
        {remainder, 3661},
        {slices, []}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1577836800, 1578268800, #options{scale = days, is_dynamic = false, is_zero_based = false}],
      expected_result, [
        {type, days},
        {count, 5},
        {from, 1577836800},
        {to, 1578268800},
        {remainder, 0},
        {slices, [
          [{from, 1577836800}, {to, 1577836800 + 86400}],
          [{from, 1577836800 + 86400}, {to, 1577836800 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400 + 86400}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578268801 - 06 Jan 2020 00:00:01 GMT
      input, [1577836800, 1578268801, #options{scale = days, is_dynamic = false, is_zero_based = false}],
      expected_result, [
        {type, days},
        {count, 5},
        {from, 1577836800},
        {to, 1578268800},
        {remainder, 1},
        {slices, [
          [{from, 1577836800}, {to, 1577836800 + 86400}],
          [{from, 1577836800 + 86400}, {to, 1577836800 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400 + 86400}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578268861 - 06 Jan 2020 00:01:01 GMT
      input, [1577836800, 1578268861, #options{scale = days, is_dynamic = false, is_zero_based = false}],
      expected_result, [
        {type, days},
        {count, 5},
        {from, 1577836800},
        {to, 1578268800},
        {remainder, 61},
        {slices, [
          [{from, 1577836800}, {to, 1577836800 + 86400}],
          [{from, 1577836800 + 86400}, {to, 1577836800 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400}],
          [{from, 1577836800 + 86400 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400 + 86400}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578272461 - 06 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578272461, #options{scale = days, is_dynamic = false, is_zero_based = false}],
      expected_result, [
      {type, days},
      {count, 5},
      {from, 1577836800},
      {to, 1578268800},
      {remainder, 3661},
      {slices, [
        [{from, 1577836800}, {to, 1577836800 + 86400}],
        [{from, 1577836800 + 86400}, {to, 1577836800 + 86400 + 86400}],
        [{from, 1577836800 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400}],
        [{from, 1577836800 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400}],
        [{from, 1577836800 + 86400 + 86400 + 86400 + 86400}, {to, 1577836800 + 86400 + 86400 + 86400 + 86400 + 86400}]
      ]}
    ]
    }
  ],

  [
    ?test(fun(_) -> {timeout, 1000, test(TestCase)} end) ||
    TestCase <- TestCases
  ].
