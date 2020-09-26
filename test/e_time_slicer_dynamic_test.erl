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
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      input, [1577836800, 1577836801, [{dynamic, true}, {max_scale, weeks}]],
      expected_result, [
        {seconds, 1},
        {slices, [
          [{type, seconds}, {from, 1577836800}, {to, 1577836801}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836861 - 01 Jan 2020 00:01:01 GMT
      input, [1577836800, 1577836861, [{dynamic, true}, {max_scale, weeks}]],
      expected_result, [
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type, minutes}, {from, 1577836800}, {to, 1577836860}],
          [{type, seconds}, {from, 1577836860}, {to, 1577836861}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577840461 - 01 Jan 2020 01:01:01 GMT
      input, [1577836800, 1577840461, [{dynamic, true}, {max_scale, weeks}]],
      expected_result, [
        {hours, 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type, hours}, {from, 1577836800}, {to, 1577836800 + 3600}],
          [{type, minutes}, {from, 1577836800 + 3600}, {to, 1577836800 + 3600 + 60}],
          [{type, seconds}, {from, 1577836800 + 3600 + 60}, {to, 1577836800 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577926861 - 02 Jan 2020 01:01:01 GMT
      input, [1577836800, 1577926861, [{dynamic, true}, {max_scale, weeks}]],
      expected_result, [
        {days, 1},
        {hours, 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type, days}, {from, 1577836800}, {to, 1577923200}],
          [{type, hours}, {from, 1577923200}, {to, 1577923200 + 3600}],
          [{type, minutes}, {from, 1577923200 + 3600}, {to, 1577923200 + 3600 + 60}],
          [{type, seconds}, {from, 1577923200 + 3600 + 60}, {to, 1577923200 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, [{dynamic, true}, {max_scale, weeks}]],
      expected_result, [
        {weeks, 1},
        {days, 1},
        {hours, 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type, weeks}, {from, 1577836800}, {to, 1577836800 + 7 * 24 * 60 * 60}],
          [{type, days}, {from, 1577836800 + 7 * 24 * 60 * 60}, {to, 1577836800 + 8 * 24 * 60 * 60}],
          [{type, hours}, {from, 1577836800 + 8 * 24 * 60 * 60}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600}],
          [{type, minutes}, {from, 1577836800 + 8 * 24 * 60 * 60 + 3600}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}],
          [{type, seconds}, {from, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, [{dynamic, true}, {max_scale, days}]],
      expected_result, [
        {days, 8},
        {hours, 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type, days}, {from, 1577836800}, {to, 1577836800 + 8 * 24 * 60 * 60}],
          [{type, hours}, {from, 1577836800 + 8 * 24 * 60 * 60}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600}],
          [{type, minutes}, {from, 1577836800 + 8 * 24 * 60 * 60 + 3600}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}],
          [{type, seconds}, {from, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, [{dynamic, true}, {max_scale, hours}]],
      expected_result, [
        {hours, 8 * 24 + 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type, hours}, {from, 1577836800}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600}],
          [{type, minutes}, {from, 1577836800 + 8 * 24 * 60 * 60 + 3600}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}],
          [{type, seconds}, {from, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, [{dynamic, true}, {max_scale, minutes}]],
      expected_result, [
        {minutes, 8 * 24 * 60 + 60 + 1},
        {seconds, 1},
        {slices, [
          [{type, minutes}, {from, 1577836800}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}],
          [{type, seconds}, {from, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, [{dynamic, true}, {max_scale, seconds}]],
      expected_result, [
        {seconds, 8 * 24 * 60 * 60 + 60 * 60 + 60 + 1},
        {slices, [
          [{type, seconds}, {from, 1577836800}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1262307661 - 01 Jan 2010 01:01:01 GMT
      %% 2524611661 - 01 Jan 2050 01:01:01 GMT
      input, [1262307661, 2524611661, [{dynamic, true}, {max_scale, weeks}]],
      expected_result, [
        {weeks, 2087},
        {days, 1},
        {slices, [
          [{type, weeks}, {from, 1262307661}, {to, 1262307661 + 2087 * 7 * 24 * 60 * 60}],
          [{type, days}, {from, 1262307661 + 2087 * 7 * 24 * 60 * 60}, {to, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60}]
        ]}
      ]
    },
    {
      %% 1262307661 - 01 Jan 2010 01:01:01 GMT
      %% 2524615322 - 01 Jan 2050 02:02:02 GMT
      input, [1262307661, 2524615322, [{dynamic, true}, {max_scale, weeks}]],
      expected_result, [
        {weeks, 2087},
        {days, 1},
        {hours, 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type, weeks}, {from, 1262307661}, {to, 1262307661 + 2087 * 7 * 24 * 60 * 60}],
          [{type, days}, {from, 1262307661 + 2087 * 7 * 24 * 60 * 60}, {to, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60}],
          [{type, hours}, {from, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60}, {to, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60 + 3600}],
          [{type, minutes}, {from, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60 + 3600}, {to, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60+ 3600 + 60}],
          [{type, seconds}, {from, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60+ 3600 + 60}, {to, 1262307661 + 2087 * 7 * 24 * 60 * 60 + 24 * 60 * 60 + 3600 + 60 + 1}]
        ]}
      ]
    }
  ],

  [
    ?test(fun(_) -> {timeout, 1000, test(TestCase)} end) ||
    TestCase <- TestCases
  ].
