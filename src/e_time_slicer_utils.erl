-module(e_time_slicer_utils).

%% API
-export([
  slice/3,

  minutes_to_weeks/1,
  minutes_to_days/1,
  minutes_to_hours/1,

  weeks_to_minutes/1,
  days_to_minutes/1,
  hours_to_minutes/1
]).

slice(_From, 0, _Step) -> [];
slice(From, AmountOfSlices, Step) ->
  do_slice(From, AmountOfSlices, Step, []).

do_slice(From, 1, Step, Acc) ->
  NewSlice = new_slice(From, From + Step),
  Res = [NewSlice | Acc],

  lists:reverse(Res);
do_slice(From, AmountOfSlices, Step, Acc) ->
  NewSlice = new_slice(From, From + Step),

  do_slice(From + Step, AmountOfSlices - 1, Step, [NewSlice|Acc]).

new_slice(From, To) ->
  [{from, From}, {to, To}].

minutes_to_weeks(Minutes) ->
  Minutes div (60 * 24 * 7).
minutes_to_days(Minutes) ->
  Minutes div (60 * 24).
minutes_to_hours(Minutes) ->
  Minutes div 60.

weeks_to_minutes(Weeks) ->
  Weeks * (60 * 24 * 7).
days_to_minutes(Days) ->
  Days * (60 * 24).
hours_to_minutes(Hours) ->
  Hours * 60.

