-module(e_time_slicer_minutes).

%% API
-export([
  slice/2
]).

slice(From0, To0) when From0 < To0 ->
  AmountOfSecondsWithinMinute = 60,
  Seconds = To0 - From0,
  AmountOfMinutes = Seconds div AmountOfSecondsWithinMinute,

  Slices = e_time_slicer_utils:slice(From0, AmountOfMinutes, AmountOfSecondsWithinMinute),

  {to, To} = if
    AmountOfMinutes > 0 -> {to, From0 + AmountOfMinutes * AmountOfSecondsWithinMinute};
    true -> {to, undefined}
  end,

  {remainder, Remainder} = case To of
    undefined -> {remainder, To0 - From0};
    To1 -> {remainder, To0 - To1}
  end,

  [
    {type, minutes},
    {count, AmountOfMinutes},
    {from, From0},
    {to, To},
    {remainder, Remainder},
    {slices, Slices}
  ].