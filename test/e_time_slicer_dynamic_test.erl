-module(e_time_slicer_dynamic_test).
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
      %% 1253968200 - 26 Sep 2009 12:30:00 GMT
      %% 1255186800 - 10 Oct 2009 15:00:00 GMT
      input, [1253968200, 1255186800, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {weeks,1},
        {days,6},
        {hours,26},
        {minutes,30},
        {slices, [
            [{type,minutes},{from,1253968200},{to,1253970000}],
            [{type,hours},{from,1253970000},{to,1254009600}],
            [{type,days},{from,1254009600},{to,1254096000}],
            [{type,weeks},{from,1254096000},{to,1254700800}],
            [{type,days},{from,1254700800},{to,1255132800}],
            [{type,hours},{from,1255132800},{to,1255186800}]
        ]}
      ]
    },
    {
      %% 1253968200 - 26 Sep 2009 12:30:00 GMT
      %% 1255186800 - 10 Oct 2009 15:00:00 GMT
      input, [1253968200, 1255186800, #options{scale = days, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {days,13},
        {hours,26},
        {minutes,30},
        {slices,[
            [{type,minutes},{from,1253968200},{to,1253970000}],
            [{type,hours},{from,1253970000},{to,1254009600}],
            [{type,days},{from,1254009600},{to,1255132800}],
            [{type,hours},{from,1255132800},{to,1255186800}]
        ]}
      ]
    },
    {
      %% 1253968200 - 26 Sep 2009 12:30:00 GMT
      %% 1255186800 - 10 Oct 2009 15:00:00 GMT
      input, [1253968200, 1255186800, #options{scale = hours, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {hours,338},
        {minutes,30},
        {slices,[
          [{type,minutes},{from,1253968200},{to,1253970000}],
          [{type,hours},{from,1253970000},{to,1255186800}]
        ]}
      ]
    },
    {
      %% 1253968200 - 26 Sep 2009 12:30:00 GMT
      %% 1255186800 - 10 Oct 2009 15:00:00 GMT
      input, [1253968200, 1255186800, #options{scale = minutes, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {minutes,20310},
        {slices,[
          [{type,minutes}, {from,1253968200}, {to,1255186800}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      input, [1577836800, 1577836801, #options{scale = weeks, is_dynamic = true, is_zero_based = false}],
      expected_result, [
        {seconds, 1},
        {slices, [
          [{type, seconds}, {from, 1577836800}, {to, 1577836801}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836801 - 01 Jan 2020 00:00:01 GMT
      input, [1577836800, 1577836801, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
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
      input, [1577836800, 1577836861, #options{scale = weeks, is_dynamic = true, is_zero_based = false}],
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
      %% 1577836861 - 01 Jan 2020 00:01:01 GMT
      input, [1577836800, 1577836861, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
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
      input, [1577836800, 1577840461, #options{scale = weeks, is_dynamic = true, is_zero_based = false}],
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
      %% 1577840461 - 01 Jan 2020 01:01:01 GMT
      input, [1577836800, 1577840461, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
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
      input, [1577836800, 1577926861, #options{scale = weeks, is_dynamic = true, is_zero_based = false}],
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
      %% 1577926861 - 02 Jan 2020 01:01:01 GMT
      input, [1577836800, 1577926861, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
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
      input, [1577836800, 1578531661, #options{scale = weeks, is_dynamic = true, is_zero_based = false}],
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
      input, [1577836800, 1578531661, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {days, 8},
        {hours, 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type,days},{from,1577836800},{to,1578268800}],
          [{type,days},{from,1578268800},{to,1578528000}],
          [{type,hours},{from,1578528000},{to,1578531600}],
          [{type,minutes},{from,1578531600},{to,1578531660}],
          [{type,seconds},{from,1578531660},{to,1578531661}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, #options{scale = days, is_dynamic = true, is_zero_based = false}],
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
      input, [1577836800, 1578531661, #options{scale = days, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {days, 8},
        {hours, 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type,days},{from,1577836800},{to,1578528000}],
          [{type,hours},{from,1578528000},{to,1578531600}],
          [{type,minutes},{from,1578531600},{to,1578531660}],
          [{type,seconds},{from,1578531660},{to,1578531661}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, #options{scale = hours, is_dynamic = true, is_zero_based = false}],
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
      input, [1577836800, 1578531661, #options{scale = hours, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {hours, 8 * 24 + 1},
        {minutes, 1},
        {seconds, 1},
        {slices, [
          [{type,hours},{from,1577836800},{to,1578531600}],
          [{type,minutes},{from,1578531600},{to,1578531660}],
          [{type,seconds},{from,1578531660},{to,1578531661}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, #options{scale = minutes, is_dynamic = true, is_zero_based = false}],
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
      input, [1577836800, 1578531661, #options{scale = minutes, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {minutes, 8 * 24 * 60 + 60 + 1},
        {seconds, 1},
        {slices, [
          [{type,minutes},{from,1577836800},{to,1578531660}],
          [{type,seconds},{from,1578531660},{to,1578531661}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, #options{scale = seconds, is_dynamic = true, is_zero_based = false}],
      expected_result, [
        {seconds, 8 * 24 * 60 * 60 + 60 * 60 + 60 + 1},
        {slices, [
          [{type, seconds}, {from, 1577836800}, {to, 1577836800 + 8 * 24 * 60 * 60 + 3600 + 60 + 1}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1578531661 - 09 Jan 2020 01:01:01 GMT
      input, [1577836800, 1578531661, #options{scale = seconds, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {seconds, 8 * 24 * 60 * 60 + 60 * 60 + 60 + 1},
        {slices, [
          [{type,seconds}, {from,1577836800}, {to,1578531661}]
        ]}
      ]
    },
    {
      %% 1262307661 - 01 Jan 2010 01:01:01 GMT
      %% 2524611661 - 01 Jan 2050 01:01:01 GMT
      input, [1262307661, 2524611661, #options{scale = weeks, is_dynamic = true, is_zero_based = false}],
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
      %% 2524611661 - 01 Jan 2050 01:01:01 GMT
      input, [1262307661, 2524611661, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {weeks,2086},
        {days,7},
        {hours,23},
        {minutes,59},
        {seconds,60},
        {slices,[[{type,seconds},{from,1262307661},{to,1262307720}],
          [{type,minutes},{from,1262307720},{to,1262311200}],
          [{type,hours},{from,1262311200},{to,1262390400}],
          [{type,days},{from,1262390400},{to,1262563200}],
          [{type,weeks},{from,1262563200},{to,2524176000}],
          [{type,days},{from,2524176000},{to,2524608000}],
          [{type,hours},{from,2524608000},{to,2524611600}],
          [{type,minutes},{from,2524611600},{to,2524611660}],
          [{type,seconds},{from,2524611660},{to,2524611661}]
        ]
      }]
    },
    {
      %% 1262307661 - 01 Jan 2010 01:01:01 GMT
      %% 2524615322 - 01 Jan 2050 02:02:02 GMT
      input, [1262307661, 2524615322, #options{scale = weeks, is_dynamic = true, is_zero_based = false}],
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
    },
    {
      %% 1262307661 - 01 Jan 2010 01:01:01 GMT
      %% 2524615322 - 01 Jan 2050 02:02:02 GMT
      input, [1262307661, 2524615322, #options{scale = weeks, is_dynamic = true, is_zero_based = true}],
      expected_result, [
        {weeks,2086},
        {days,7},
        {hours,24},
        {minutes,60},
        {seconds,61},
        {slices,[
          [{type,seconds},{from,1262307661},{to,1262307720}],
          [{type,minutes},{from,1262307720},{to,1262311200}],
          [{type,hours},{from,1262311200},{to,1262390400}],
          [{type,days},{from,1262390400},{to,1262563200}],
          [{type,weeks},{from,1262563200},{to,2524176000}],
          [{type,days},{from,2524176000},{to,2524608000}],
          [{type,hours},{from,2524608000},{to,2524615200}],
          [{type,minutes},{from,2524615200},{to,2524615320}],
          [{type,seconds},{from,2524615320},{to,2524615322}]
        ]}
      ]
    }
  ],

  [
    ?test(fun(_) -> {timeout, 1000, test(TestCase)} end) ||
    TestCase <- TestCases
  ].
