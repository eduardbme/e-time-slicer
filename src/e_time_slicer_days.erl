-module(e_time_slicer_days).

%% API
-export([
  slice/2
]).

slice(From, From) ->
  [
    {type, days},
    {count, 0},
    {from, From},
    {to, From},
    {remainder, 0},
    {slices, []}
  ];
slice(From0, To0) when From0 < To0 ->
  AmountOfSecondsWithinDay = 86400,
  Seconds = To0 - From0,
  AmountOfDays = Seconds div AmountOfSecondsWithinDay,

  Slices = e_time_slicer_utils:slice(From0, AmountOfDays, AmountOfSecondsWithinDay),

  {to, To} = if
    AmountOfDays > 0 -> {to, From0 + AmountOfDays * AmountOfSecondsWithinDay};
    true -> {to, undefined}
  end,

  {remainder, Remainder} = case To of
    undefined -> {remainder, To0 - From0};
    To1 -> {remainder, To0 - To1}
  end,

  [
    {type, days},
    {count, AmountOfDays},
    {from, From0},
    {to, To},
    {remainder, Remainder},
    {slices, Slices}
  ].