-module(e_time_slicer_calendar).

%% API
-export([
  ceil/2,
  seconds_to_date/1
]).

-define(EPOCH, calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).

ceil(From, week) ->
  %% current date based on 'From' timestamp
  {Date, Time} = seconds_to_date(From),

  %% day of the week for that date
  DayOfTheWeek = calendar:day_of_the_week(Date),

  Res = case {DayOfTheWeek, Time}  of
    {1, {0, 0, 0}} ->
      %% this is just the begging of the current week
      %% Monday with 00:00:00
      %%
      %% math:ceil(2.0) -> 2.0
      %% so no reason to make it different for week
      From;
    _ ->
      %% days left to the next week
      DayToTheEndOfTheWeek = 7 - DayOfTheWeek,

      %% calculate the number of next week's first day
      CurrentDays = calendar:date_to_gregorian_days(Date),
      %% +1 here is a little magic
      %%
      %% CurrentDays + DayToTheEndOfTheWeek will return the last day of the current week,
      %% but we need to get next day after it, with 00:00:00
      CeiledDate = calendar:gregorian_days_to_date(CurrentDays + DayToTheEndOfTheWeek + 1),

      calendar:datetime_to_gregorian_seconds({CeiledDate, {0, 0, 0}}) - ?EPOCH
  end,

  Res;
ceil(From, day) ->
  %% current date based on 'From' timestamp
  {Date, Time} = seconds_to_date(From),

  Res = case Time  of
    {0, 0, 0} ->
      %% this is just the begging of the day
      %% 00:00:00
      %%
      %% math:ceil(2.0) -> 2.0
      %% so no reason to make it different for week
      From;
    _ ->
      %% calculate the number of next week's first day
      CurrentDays = calendar:date_to_gregorian_days(Date),
      %% +1 here is a little magic
      %%
      %% CurrentDays will return this particular day,
      %% but we need to get next day after it, with 00:00:00
      CeiledDate = calendar:gregorian_days_to_date(CurrentDays + 1),

      calendar:datetime_to_gregorian_seconds({CeiledDate, {0, 0, 0}}) - ?EPOCH
  end,

  Res;
ceil(From, hour) ->
  %% current date based on 'From' timestamp
  {Date, Time} = seconds_to_date(From),

  Res = case Time  of
    {_Hour, 0, 0} ->
      %% this is just the begging of the hour
      %% 00:00:00
      %%
      %% math:ceil(2.0) -> 2.0
      %% so no reason to make it different for week
      From;
    {23, _Minutes, _Seconds} ->
      %% ceil(23:XX:XX) is just a next day with 00:00:00
      CurrentDays = calendar:date_to_gregorian_days(Date),

      %% calculate the number of next week's first day
      CurrentDays = calendar:date_to_gregorian_days(Date),
      %% +1 here is a little magic
      %%
      %% CurrentDays will return this particular day,
      %% but we need to get next day after it, with 00:00:00
      CeiledDate = calendar:gregorian_days_to_date(CurrentDays + 1),

      calendar:datetime_to_gregorian_seconds({CeiledDate, {0, 0, 0}}) - ?EPOCH;
    {Hours, _Minutes, _Seconds} ->
      %% ceil(22:XX:XX) is the same day with 23:00:00
      calendar:datetime_to_gregorian_seconds({Date, {Hours + 1, 0, 0}}) - ?EPOCH
  end,

  Res;
ceil(From, minute) ->
  %% current date based on 'From' timestamp
  {Date, Time} = seconds_to_date(From),

  Res = case Time  of
          {_Hours, _Minute, 0} ->
            %% this is just the begging of the hour
            %% 00:00:00
            %%
            %% math:ceil(2.0) -> 2.0
            %% so no reason to make it different for week
            From;
          {23, 59, _Seconds} ->
            %% ceil(23:59:XX) is just a next day with 00:00:00
            CurrentDays = calendar:date_to_gregorian_days(Date),

            %% calculate the number of next week's first day
            CurrentDays = calendar:date_to_gregorian_days(Date),
            %% +1 here is a little magic
            %%
            %% CurrentDays will return this particular day,
            %% but we need to get next day after it, with 00:00:00
            CeiledDate = calendar:gregorian_days_to_date(CurrentDays + 1),

            calendar:datetime_to_gregorian_seconds({CeiledDate, {0, 0, 0}}) - ?EPOCH;
          {Hours, 59, _Seconds} ->
            %% ceil(22:59:XX) is the same day, but next hour 23:00:00
            calendar:datetime_to_gregorian_seconds({Date, {Hours + 1, 0, 0}}) - ?EPOCH;
          {Hours, Minutes, _Seconds} ->
            %% ceil(22:XX:XX) is the same day with 23:00:00
            calendar:datetime_to_gregorian_seconds({Date, {Hours, Minutes + 1, 0}}) - ?EPOCH
        end,

  Res.

seconds_to_date(From) ->
  BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Seconds = BaseDate + From,
  calendar:gregorian_seconds_to_datetime(Seconds).
