-module(e_time_slicer_seconds_test).
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
      input, [1577836800, 1577836801, #options{scale = seconds, is_dynamic = false, is_zero_based = false}],
      expected_result, [
        {type, seconds},
        {count, 1},
        {from, 1577836800},
        {to, 1577836801},
        {remainder, 0},
        {slices, [
          [{from, 1577836800},{to, 1577836801}]
        ]}
      ]
    },
    {
      %% 1577836800 - 01 Jan 2020 00:00:00 GMT
      %% 1577836861 - 01 Jan 2020 00:01:01 GMT
      input, [1577836800, 1577836861, #options{scale = seconds, is_dynamic = false, is_zero_based = false}],
      expected_result, [
        {type, seconds},
        {count, 60 + 1},
        {from, 1577836800},
        {to, 1577836861},
        {remainder, 0},
        {slices, [
          [{from,1577836800},{to,1577836801}],
          [{from,1577836801},{to,1577836802}],
          [{from,1577836802},{to,1577836803}],
          [{from,1577836803},{to,1577836804}],
          [{from,1577836804},{to,1577836805}],
          [{from,1577836805},{to,1577836806}],
          [{from,1577836806},{to,1577836807}],
          [{from,1577836807},{to,1577836808}],
          [{from,1577836808},{to,1577836809}],
          [{from,1577836809},{to,1577836810}],
          [{from,1577836810},{to,1577836811}],
          [{from,1577836811},{to,1577836812}],
          [{from,1577836812},{to,1577836813}],
          [{from,1577836813},{to,1577836814}],
          [{from,1577836814},{to,1577836815}],
          [{from,1577836815},{to,1577836816}],
          [{from,1577836816},{to,1577836817}],
          [{from,1577836817},{to,1577836818}],
          [{from,1577836818},{to,1577836819}],
          [{from,1577836819},{to,1577836820}],
          [{from,1577836820},{to,1577836821}],
          [{from,1577836821},{to,1577836822}],
          [{from,1577836822},{to,1577836823}],
          [{from,1577836823},{to,1577836824}],
          [{from,1577836824},{to,1577836825}],
          [{from,1577836825},{to,1577836826}],
          [{from,1577836826},{to,1577836827}],
          [{from,1577836827},{to,1577836828}],
          [{from,1577836828},{to,1577836829}],
          [{from,1577836829},{to,1577836830}],
          [{from,1577836830},{to,1577836831}],
          [{from,1577836831},{to,1577836832}],
          [{from,1577836832},{to,1577836833}],
          [{from,1577836833},{to,1577836834}],
          [{from,1577836834},{to,1577836835}],
          [{from,1577836835},{to,1577836836}],
          [{from,1577836836},{to,1577836837}],
          [{from,1577836837},{to,1577836838}],
          [{from,1577836838},{to,1577836839}],
          [{from,1577836839},{to,1577836840}],
          [{from,1577836840},{to,1577836841}],
          [{from,1577836841},{to,1577836842}],
          [{from,1577836842},{to,1577836843}],
          [{from,1577836843},{to,1577836844}],
          [{from,1577836844},{to,1577836845}],
          [{from,1577836845},{to,1577836846}],
          [{from,1577836846},{to,1577836847}],
          [{from,1577836847},{to,1577836848}],
          [{from,1577836848},{to,1577836849}],
          [{from,1577836849},{to,1577836850}],
          [{from,1577836850},{to,1577836851}],
          [{from,1577836851},{to,1577836852}],
          [{from,1577836852},{to,1577836853}],
          [{from,1577836853},{to,1577836854}],
          [{from,1577836854},{to,1577836855}],
          [{from,1577836855},{to,1577836856}],
          [{from,1577836856},{to,1577836857}],
          [{from,1577836857},{to,1577836858}],
          [{from,1577836858},{to,1577836859}],
          [{from,1577836859},{to,1577836860}],
          [{from,1577836860},{to,1577836861}]
        ]}
      ]
    }
  ],

  [
    ?test(fun(_) -> {timeout, 1000, test(TestCase)} end) ||
    TestCase <- TestCases
  ].
