-module(e_time_slice).

%% API
-export([
  slice/3
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