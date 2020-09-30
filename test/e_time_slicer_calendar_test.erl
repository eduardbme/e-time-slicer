-module(e_time_slicer_calendar_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(test(T), {setup, fun start/0, fun stop/1, T}).

start() -> ok.
stop(_) -> ok.

test({input, Input, expected_result, ExpectedResult}) ->
  fun() ->
    Result = erlang:apply(e_time_slicer_calendar, ceil, Input),

    ?assertEqual(ExpectedResult, Result)
  end.

all_tests_test_() ->
  TestCases = [
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1577836800, week],
      expected_result, 1578268800
    },
    {
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1577836801, week],
      expected_result, 1578268800
    },
    {
      %% 1577836861 - 01 Jan 2020 00:01:01 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1577836861, week],
      expected_result, 1578268800
    },
    {
      %% 1577840461 - 01 Jan 2020 01:01:01 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1577840461, week],
      expected_result, 1578268800
    },
    {
      %% 1577923200 - 02 Jan 2020 00:00:00 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1577923200, week],
      expected_result, 1578268800
    },
    {
      %% 1578009600 - 03 Jan 2020 00:00:00 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1578009600, week],
      expected_result, 1578268800
    },
    {
      %% 1578096000 - 04 Jan 2020 00:00:00 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1578096000, week],
      expected_result, 1578268800
    },
    {
      %% 1578182400 - 05 Jan 2020 00:00:00 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1578182400, week],
      expected_result, 1578268800
    },
    {
      %% 1578268799 - 05 Jan 2020 23:59:59 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1578268799, week],
      expected_result, 1578268800
    },
    {
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      %% 1578268800 - 06 Jan 2020 00:00:00 GMT
      input, [1578268800, week],
      expected_result, 1578268800
    },
    {
      %% 1578268801 - 06 Jan 2020 00:00:01 GMT
      %% 1578873600 - 13 Jan 2020 00:00:00 GMT
      input, [1578268801, week],
      expected_result, 1578873600
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      input, [1577836800, day],
      expected_result, 1577836800
    },
    {
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      %% 1577923200 - 02 Jan 2020 00:00:00 GMT
      input, [1577836801, day],
      expected_result, 1577923200
    },
    {
      %% 1577923199 - 01 Jan 2020 23:59:59 GMT
      %% 1577923200 - 02 Jan 2020 00:00:00 GMT
      input, [1577923199, day],
      expected_result, 1577923200
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      input, [1577836800, hour],
      expected_result, 1577836800
    },
    {
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      %% 1577840400 - 01 Jan 2020 01:00:00 GMT
      input, [1577836801, hour],
      expected_result, 1577840400
    },
    {
      %% 1577836801 - 01 Jan 2020 00:01:00 GMT
      %% 1577840400 - 01 Jan 2020 01:00:00 GMT
      input, [1577836801, hour],
      expected_result, 1577840400
    },
    {
      %% 1577923199 - 01 Jan 2020 23:59:59 GMT
      %% 1577923200 - 02 Jan 2020 00:00:00 GMT
      input, [1577923199, hour],
      expected_result, 1577923200
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      input, [1577836800, minute],
      expected_result, 1577836800
    },
    {
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      %% 1577836860 - 01 Jan 2020 00:01:00 GMT
      input, [1577836801, minute],
      expected_result, 1577836860
    },
    {
      %% 1577836860 - 01 Jan 2020 00:01:00 GMT
      %% 1577836860 - 01 Jan 2020 00:01:00 GMT
      input, [1577836860, minute],
      expected_result, 1577836860
    },
    {
      %% 1577836861 - 01 Jan 2020 00:01:01 GMT
      %% 1577836920 - 01 Jan 2020 00:02:00 GMT
      input, [1577836861, minute],
      expected_result, 1577836920
    },
    {
      %% 1577923199 - 01 Jan 2020 23:59:59 GMT
      %% 1577923200 - 02 Jan 2020 00:00:00 GMT
      input, [1577923199, minute],
      expected_result, 1577923200
    }
  ],

  [
    ?test(fun(_) -> {timeout, 1000, test(TestCase)} end) ||
    TestCase <- TestCases
  ].
