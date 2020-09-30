-module(e_time_slicer).

-include("include/e_time_slicer.hrl").

%% API
-export([
  slice/2,
  slice/3
]).

-spec slice(From, To) -> {ok, Result} when
  From :: non_neg_integer(),
  To :: non_neg_integer(),
  Result :: [{_,_},...].
slice(From, To) ->
  slice(From, To, #options{}).

-spec slice(From, To, Options) -> {ok, Result} when
  From :: non_neg_integer(),
  To :: non_neg_integer(),
  Options :: #options{},
  Result :: [{_,_},...].
slice(From, To, #options{is_zero_based = IsZeroBased} = Options) ->
  TimeRanges = case IsZeroBased of
    true -> do_slice_zero_based(From, To, Options);
    false -> do_slice(From, To, Options)
  end,

  {ok, TimeRanges}.

do_slice_zero_based(From, To, Options) ->
  TimeRanges = do_slice_zero_based(From, To, Options, []),

  NormalizedTimeRange = normalize(TimeRanges),

  %% assert that verifies that amount of seconds within
  %% `NormalizedTimeRange` is equals to `From - To`
  e_time_slicer_verifier:verify(From, To, NormalizedTimeRange),

  NormalizedTimeRange.

do_slice_zero_based(From, To, #options{scale = minutes} = Options, Acc) ->
  {ok, Slices} = slice(From, To, Options#options{is_zero_based = false}),

  merge([Slices|Acc]);
do_slice_zero_based(From, To, #options{scale = Scale} = Options, Acc) ->
  RoundedFrom = e_time_slicer_calendar:ceil(From, scale_to_calendar_term(Scale)),

  {ok, Slices} = slice(RoundedFrom, To, Options#options{is_zero_based = false}),

  do_slice_zero_based(From, RoundedFrom, Options#options{scale = next_scale(Scale)}, [Slices|Acc]).

do_slice(weeks, From, To) ->
  e_time_slicer_weeks:slice(From, To);
do_slice(days, From, To) ->
  e_time_slicer_days:slice(From, To);
do_slice(hours, From, To) ->
  e_time_slicer_hours:slice(From, To);
do_slice(minutes, From, To) ->
  e_time_slicer_minutes:slice(From, To);
do_slice(seconds, From, To) ->
  e_time_slicer_seconds:slice(From, To);
do_slice(From, To, #options{scale = Scale, is_dynamic = false}) ->
  do_slice(Scale, From, To);
do_slice(From, To, #options{scale = Scale, is_dynamic = true}) ->
  TimeRanges = do_slice_with_acc(Scale, From, To, [], []),

  NormalizedTimeRange = normalize(TimeRanges),

  %% assert that verifies that amount of seconds within
  %% `NormalizedTimeRange` is equals to `From - To`
  e_time_slicer_verifier:verify(From, To, NormalizedTimeRange),

  TimeRanges.

do_slice_with_acc(undefined, _From, _To, TotalAcc0, SlicesAcc0) ->
  SlicesAcc = lists:reverse(SlicesAcc0),
  TotalAcc1 = [{slices, SlicesAcc}|TotalAcc0],

  lists:reverse(TotalAcc1);
do_slice_with_acc(Scale, From0, To0, TotalAcc0, SlicesAcc0) ->
  Res = do_slice(Scale, From0, To0),

  Slices = proplists:get_value(slices, Res),
  Count = proplists:get_value(count, Res),
  From = if
    Count > 0 -> proplists:get_value(to, Res);
    true -> From0
  end,

  TotalAcc = if
    Count > 0 -> [{Scale, Count}|TotalAcc0];
    true -> TotalAcc0
  end,

  SlicesAcc = case slice_entire_range(Slices) of
    [] -> SlicesAcc0;
    [{from, _From}, {to, _To}] = Range ->
      NewSlice = [{type, Scale}|Range],

      [NewSlice|SlicesAcc0]
  end,

  Remainder = proplists:get_value(remainder, Res),

  if
    Remainder > 0 -> do_slice_with_acc(next_scale(Scale), From, To0, TotalAcc, SlicesAcc);
    true -> do_slice_with_acc(undefined, From, To0, TotalAcc, SlicesAcc)
  end.

next_scale(weeks) -> days;
next_scale(days) -> hours;
next_scale(hours) -> minutes;
next_scale(minutes) -> seconds;
next_scale(seconds) -> undefined.

scale_to_calendar_term(weeks) -> week;
scale_to_calendar_term(days) -> day;
scale_to_calendar_term(hours) -> hour;
scale_to_calendar_term(minutes) -> minute.

slice_entire_range(Slices) ->
  case Slices of
    [] -> [];
    [Item] -> Item;
    [FirstElement|Rest] ->
      LastElement = lists:last(Rest),

      From = proplists:get_value(from, FirstElement),
      To = proplists:get_value(to, LastElement),

      [{from, From}, {to, To}]
  end.

merge([TimeRange1|[]]) -> TimeRange1;
merge([TimeRange1|[TimeRange2|TimeRanges]]) ->
  merge([merge_slices(TimeRange1, TimeRange2)|TimeRanges]).

merge_slices([], TimeRange2) -> TimeRange2;
merge_slices([Record|TimeRange1], TimeRange2) ->
  merge_slices(TimeRange1, merge_metric(Record, TimeRange2)).

merge_metric({Metric, MetricValue}, TimeRange2) ->
  case {Metric, proplists:get_value(Metric, TimeRange2)} of
    {Metric, undefined} ->
      [{Metric, MetricValue} | TimeRange2];
    {slices, MetricValueFromTimeRange2} ->
      [{slices, MetricValue ++ MetricValueFromTimeRange2}|proplists:delete(slices, TimeRange2)];
    {Metric, MetricValueFromTimeRange2} ->
      [{Metric, MetricValue + MetricValueFromTimeRange2}|proplists:delete(Metric, TimeRange2)]
  end.

normalize(TimeRange) ->
  Minutes = proplists:get_value(minutes, TimeRange),
  Hours = proplists:get_value(hours, TimeRange),
  Days = proplists:get_value(days, TimeRange),
  Weeks = proplists:get_value(weeks, TimeRange),
  Slices = proplists:get_value(slices, TimeRange),

  [
    {Key, Value} || {Key, Value} <-
    [{minutes, Minutes}, {hours, Hours}, {days, Days}, {weeks, Weeks}, {slices, Slices}],
    Value =/= undefined
  ].
