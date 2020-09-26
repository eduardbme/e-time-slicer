-module(e_time_slicer).

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
  slice(From, To, []).

-spec slice(From, To, Options) -> {ok, Result} when
  From :: non_neg_integer(),
  To :: non_neg_integer(),
  Options :: proplists:proplist(),
  Result :: [{_,_},...].
slice(From, To, Options) ->
  FixedScale = proplists:get_value(scale, Options, weeks),
  StartScale = proplists:get_value(max_scale, Options, weeks),
  IsDynamic = proplists:get_value(dynamic, Options, true),

  case IsDynamic of
    true -> do_slice(StartScale, From, To, dynamic);
    false -> do_slice(FixedScale, From, To, once)
  end.

do_slice(Scale, From, To, once) ->
  do_slice(Scale, From, To);
do_slice(Scale, From, To, dynamic) ->
  do_slice_with_acc(Scale, From, To, [], []).

do_slice_with_acc(undefined, _From, _To, TotalAcc0, SlicesAcc0) ->
  SlicesAcc = lists:reverse(SlicesAcc0),
  TotalAcc1 = [{slices, SlicesAcc}|TotalAcc0],

  {ok, lists:reverse(TotalAcc1)};
do_slice_with_acc(Scale, From0, To0, TotalAcc0, SlicesAcc0) ->
  {ok, Res} = do_slice(Scale, From0, To0),

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

do_slice(weeks, From, To) ->
  e_time_slicer_weeks:slice(From, To);
do_slice(days, From, To) ->
  e_time_slicer_days:slice(From, To);
do_slice(hours, From, To) ->
  e_time_slicer_hours:slice(From, To);
do_slice(minutes, From, To) ->
  e_time_slicer_minutes:slice(From, To);
do_slice(seconds, From, To) ->
  e_time_slicer_seconds:slice(From, To).

next_scale(weeks) -> days;
next_scale(days) -> hours;
next_scale(hours) -> minutes;
next_scale(minutes) -> seconds;
next_scale(seconds) -> undefined.

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