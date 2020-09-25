-module(e_time_slicer_years).

%% API
-export([
  slice/2
]).

slice(From0, To0) when From0 < To0 ->
  AmountOfSecondsWithinYear = amount_of_seconds_for_year(From0),
  Seconds = To0 - From0,
  io:format("~p ~p", [Seconds, AmountOfSecondsWithinYear]),
  AmountOfYears = Seconds div AmountOfSecondsWithinYear,

  Slices = e_time_slice:slice(From0, AmountOfYears, AmountOfSecondsWithinYear),

  To = From0 + AmountOfYears * AmountOfSecondsWithinYear,

  {ok, [
    {type, years},
    {count, AmountOfYears},
    {from, From0},
    {to, To},
    {remainder, To0 - To},
    {slices, Slices}
  ]}.

amount_of_seconds_for_year(Timestamp) ->
  {{Year, _Month, _Day}, _Time} = adjust_date(Timestamp),

  io:format("~p ~p ~p", [Timestamp, Year, calendar:is_leap_year(Year)]),

  case calendar:is_leap_year(Year) of
    true -> 31622400;
    false -> 31536000
  end.

adjust_date(Seconds0) ->
  BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Seconds       = BaseDate + Seconds0,
  calendar:gregorian_seconds_to_datetime(Seconds).
