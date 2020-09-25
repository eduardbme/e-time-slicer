-module(e_time_slicer_weeks).

%% API
-export([
  slice/2
]).

slice(From0, To0) when From0 < To0 ->
  AmountOfSecondsWithinWeek = 604800,
  Seconds = To0 - From0,
  AmountOfWeeks = Seconds div AmountOfSecondsWithinWeek,

  Slices = e_time_slice:slice(From0, AmountOfWeeks, AmountOfSecondsWithinWeek),
  To = From0 + AmountOfWeeks * AmountOfSecondsWithinWeek,

  {ok, [
    {type, years},
    {count, AmountOfWeeks},
    {from, From0},
    {to, To},
    {remainder, To0 - To},
    {slices, Slices}
  ]}.