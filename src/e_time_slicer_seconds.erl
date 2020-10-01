-module(e_time_slicer_seconds).

%% API
-export([
  slice/2
]).

slice(From, From) ->
  [
    {type, seconds},
    {count, 0},
    {from, From},
    {to, From},
    {remainder, 0},
    {slices, []}
  ];
slice(From0, To0) when From0 < To0 ->
  Slices = e_time_slicer_utils:slice(From0, To0 - From0, 1),

  [
    {type, seconds},
    {count, To0 - From0},
    {from, From0},
    {to, To0},
    {remainder, 0},
    {slices, Slices}
  ].