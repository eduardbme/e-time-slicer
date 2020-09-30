-module(e_time_slicer_verifier).

%% API
-export([
  verify/3
]).

verify(From, To, TimeRange) ->
  Slices = proplists:get_value(slices, TimeRange),

  valid(To - From, time_range_in_slices(Slices)).

time_range_in_slices(TimeRanges) ->
  time_range_in_slices(TimeRanges, 0).

time_range_in_slices([], Acc) -> Acc;
time_range_in_slices([[{type, _Type}, {from, From}, {to, To}]|TimeRanges], Acc) ->
  time_range_in_slices(TimeRanges, Acc + (To - From)).

valid(A, A) -> ok;
valid(_A, _B) -> throw (buggy_time_slice).