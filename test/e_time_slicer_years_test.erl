-module(e_time_slicer_years_test).
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
    {
      %% Test with a leap year
      %% 1577836800 // 01 Jan 2020 00:00:00 GMT
      %% 1609459200 // 01 Jan 2021 00:00:00 GMT
      input, [1577836800, 1609459200, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 1},
        {from, 1577836800},
        {to, 1609459200},
        {remainder, 0},
        {slices, [
          [{from, 1577836800}, {to, 1609459200}]
        ]}
      ]
    },
    {
      %% Test with a leap year
      %% 1451606400 // 01 Jan 2016 00:00:00 GMT
      %% 1514764800 // 01 Jan 2018 00:00:00 GMT
      input, [1451606400, 1514764800, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 2},
        {from, 1451606400},
        {to, 1514764800},
        {remainder, 0},
        {slices, [
          [{from, 1451606400}, {to, 1546344000}],
          [{from, 1546344000}, {to, 1514764800}]
        ]}
      ]
    },
    {
      %% Test with a leap years
      %% 1262304000 // 01 Jan 2010 00:00:00 GMT
      %% 1577836800 // 01 Jan 2020 00:00:00 GMT
      input, [1262304000, 1577836800, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 10},
        {from, 1262304000},
        {to, 1577836800},
        {remainder, 0},
        {slices, []}
      ]
    },
    {
      %% 1546300800 // 01 Jan 2019 00:00:00 GMT
      %% 1577836800 // 01 Jan 2020 00:00:00 GMT
      input, [1546300800, 1577836800, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 1},
        {from, 1546300800},
        {to, 1577836800},
        {remainder, 0},
        {slices, [
          [{from, 1546300800}, {to, 1577836800}]
        ]}
      ]
    },
    {
      %% 1546300800 // 01 Jan 2019 00:00:00 GMT
      %% 1577836800 + 86400 // 02 Jan 2020 00:00:00 GMT
      input, [1546300800, 1577836800 + 86400, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 1},
        {from, 1546300800},
        {to, 1577836800},
        {remainder, 86400},
        {slices, [
          [{from, 1546300800}, {to, 1577836800}]
        ]}
      ]
    },
    {
      %% 1546300800 // 01 Jan 2019 00:00:00 GMT
      %% 1577836800 + 86400 + 3600 // 02 Jan 2020 01:00:00 GMT
      input, [1546300800, 1577836800 + 86400 + 3600, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 1},
        {from, 1546300800},
        {to, 1577836800},
        {remainder, 86400 + 3600},
        {slices, [
          [{from, 1546300800}, {to, 1577836800}]
        ]}
      ]
    },
    {
      %% 1546300800 // 01 Jan 2019 00:00:00 GMT
      %% 1577836800 + 86400 + 3600 + 3540 // 02 Jan 2020 01:59:00 GMT
      input, [1546300800, 1577836800 + 86400 + 3600 + 3540, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 1},
        {from, 1546300800},
        {to, 1577836800},
        {remainder, 86400 + 3600 + 3540},
        {slices, [
          [{from, 1546300800}, {to, 1577836800}]
        ]}
      ]
    },
    {
      %% 1546300800 // 01 Jan 2019 00:00:00 GMT
      %% 1577836800 + 86400 + 3600 + 3540 + 59 // 02 Jan 2020 01:59:59 GMT
      input, [1546300800, 1577836800 + 86400 + 3600 + 3540 + 59, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 1},
        {from, 1546300800},
        {to, 1577836800},
        {remainder, 86400 + 3600 + 3540 + 59},
        {slices, [
          [{from, 1546300800}, {to, 1577836800}]
        ]}
      ]
    },
    {
      %% 1483228800 // 01 Jan 2017 00:00:00 GMT
      %% 1546300800 // 01 Jan 2019 00:00:00 GMT
      input, [1483228800, 1546300800, [{scale, years}]],
      expected_result, [
        {type, years},
        {count, 2},
        {from, 1483228800},
        {to, 1546300800},
        {remainder, 0},
        {slices, [
          [{from, 1483228800}, {to, 1483228800 + 31536000}],
          [{from, 1483228800 + 31536000}, {to, 1546300800}]
        ]}
      ]
    }
  ],

  [
    ?test(fun(_) -> {timeout, 1000, test(TestCase)} end) ||
    TestCase <- TestCases
  ].
