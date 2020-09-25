-module(e_time_slicer).

%% API
-export([
  slice/3
]).

slice(From, To, Options) ->
  Scale = proplists:get_value(scale, Options),
  MaxScale = proplists:get_value(max_scale, Options),

  case proplists:get_value(scale, Options) of
    dynamic -> do_slice(MaxScale, From, To, dynamic);
    Scale -> do_slice(Scale, From, To, once)
  end.

do_slice(Scale, From, To, once) ->
  do_slice(Scale, From, To);
do_slice(Scale, From, To, dynamic) ->
  do_slice_with_acc(Scale, From, To, []).

do_slice_with_acc(undefined, _From, _To, Acc) ->
  Slices = proplists:get_value(slices, Acc),
  AccWithoutSlices = proplists:delete(slices, Acc),

  {ok, lists:reverse(AccWithoutSlices) ++ [{slices, lists:reverse(Slices)}]};
do_slice_with_acc(Scale, From0, To0, Acc0) ->
  {ok, Res} = do_slice(Scale, From0, To0),

  Count = proplists:get_value(Scale, Res),
  To = proplists:get_value(to, Res),

  Acc = if
    Count > 0 ->
      From = proplists:get_value(from, Res),

      CurrentSlices = proplists:get_value(slices, Acc0, []),
      Acc1 = proplists:delete(slices, Acc0),

      [
        {slices, [
          [{type, Scale}, {from, From}, {to, To}] | CurrentSlices
        ]},
        {Scale, Count} | Acc1
      ];
    true -> Acc0
  end,

  Remainder = proplists:get_value(remainder, Res),

  if
    Remainder > 0 -> do_slice_with_acc(next_scale(Scale), To, To0, Acc);
    true -> do_slice_with_acc(undefined, To, To0, Acc)
  end.

do_slice(years, From, To) ->
  e_time_slicer_years:slice(From, To);
do_slice(weeks, From, To) ->
  e_time_slicer_weeks:slice(From, To).

next_scale(years) -> weeks;
next_scale(weeks) -> days;
next_scale(days) -> hours;
next_scale(hours) -> minutes;
next_scale(minutes) -> seconds;
next_scale(seconds) -> undefined.
