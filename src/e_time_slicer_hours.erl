-module(e_time_slicer_hours).

%% API
-export([
  slice/2
]).

slice(From0, To0) when From0 < To0 ->
  AmountOfSecondsWithinHour = 3600,
  Seconds = To0 - From0,
  AmountOfHours = Seconds div AmountOfSecondsWithinHour,

  Slices = e_time_slicer_utils:slice(From0, AmountOfHours, AmountOfSecondsWithinHour),

  {to, To} = if
    AmountOfHours > 0 -> {to, From0 + AmountOfHours * AmountOfSecondsWithinHour};
    true -> {to, undefined}
  end,

  {remainder, Remainder} = case To of
    undefined -> {remainder, To0 - From0};
    To1 -> {remainder, To0 - To1}
  end,

  [
    {type, hours},
    {count, AmountOfHours},
    {from, From0},
    {to, To},
    {remainder, Remainder},
    {slices, Slices}
  ].