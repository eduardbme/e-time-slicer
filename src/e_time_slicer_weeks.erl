-module(e_time_slicer_weeks).

%% API
-export([
  slice/2
]).

slice(From0, To0) when From0 < To0 ->
  AmountOfSecondsWithinWeek = 604800,
  Seconds = To0 - From0,
  AmountOfWeeks = Seconds div AmountOfSecondsWithinWeek,

  Slices = e_time_slicer_utils:slice(From0, AmountOfWeeks, AmountOfSecondsWithinWeek),

  {to, To} = if
    AmountOfWeeks > 0 -> {to, From0 + AmountOfWeeks * AmountOfSecondsWithinWeek};
    true -> {to, undefined}
  end,

  {remainder, Remainder} = case To of
    undefined -> {remainder, To0 - From0};
    To1 -> {remainder, To0 - To1}
  end,

  [
    {type, weeks},
    {count, AmountOfWeeks},
    {from, From0},
    {to, To},
    {remainder, Remainder},
    {slices, Slices}
  ].