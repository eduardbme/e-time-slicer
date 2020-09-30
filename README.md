# e-time-slicer
[![Build Status](https://travis-ci.org/eduardbme/e-time-slicer.svg?branch=master)](https://travis-ci.org/eduardbme/e-time-slicer)
[![Coverage Status](https://coveralls.io/repos/github/eduardbme/e-time-slicer/badge.svg?branch=master)](https://coveralls.io/github/eduardbme/e-time-slicer?branch=master)


# Description
```erlang
-record(options, {
  scale = weeks,
  is_dynamic = false,
  is_zero_based = false
}).

-spec slice(From, To, Options) -> {ok, Result} when
  From :: non_neg_integer(),
  To :: non_neg_integer(),
  Options :: #options{},
  Result :: list().
slice(From, To, Options) -> ...
```

# How to use:
```erlang
%% 1577836800 - 01 Jan 2020 00:00:00 GMT
%% 1578531661 - 09 Jan 2020 01:01:01 GMT
1> e_time_slicer:slice(1577836800, 1578531661, #options{scale = weeks, is_dynamic = true}).
{ok,[{weeks,1},
     {days,1},
     {hours,1},
     {minutes,1},
     {seconds,1},
     {slices,[[{type,weeks},{from,1577836800},{to,1578441600}],      %% 1578441600 - 08 Jan 2020 00:00:00 GMT
              [{type,days},{from,1578441600},{to,1578528000}],       %% 1578528000 - 09 Jan 2020 00:00:00 GMT
              [{type,hours},{from,1578528000},{to,1578531600}],      %% 1578531600 - 09 Jan 2020 01:00:00 GMT
              [{type,minutes},{from,1578531600},{to,1578531660}],    %% 1578531660 - 09 Jan 2020 01:01:00 GMT
              [{type,seconds},{from,1578531660},{to,1578531661}]]}]} %% 1578531661 - 09 Jan 2020 01:01:01 GMT

%% When dynamic option equals to false, slice returns only one fixed type (default is weeks)

%% 1577836800 - 01 Jan 2020 00:00:00 GMT
%% 1578531661 - 09 Jan 2020 01:01:01 GMT
3> e_time_slicer:slice(1577836800, 1578531661, #options{is_dynamic = false}).
{ok,[{type,weeks},
     {count,1},
     {from,1577836800},
     {to,1578441600},
     {remainder,90061},
     {slices,[[{from,1577836800},{to,1578441600}]]}]}

%% 1577836800 - 01 Jan 2020 00:00:00 GMT
%% 1578531661 - 09 Jan 2020 01:01:01 GMT
4> e_time_slicer:slice(1577836800, 1578531661, #options{scale = days, is_dynamic = false}). 
{ok,[{type,days},
     {count,8},
     {from,1577836800},
     {to,1578528000},
     {remainder,3661},
     {slices,[[{from,1577836800},{to,1577923200}],
              [{from,1577923200},{to,1578009600}],
              [{from,1578009600},{to,1578096000}],
              [{from,1578096000},{to,1578182400}],
              [{from,1578182400},{to,1578268800}],
              [{from,1578268800},{to,1578355200}],
              [{from,1578355200},{to,1578441600}],
              [{from,1578441600},{to,1578528000}]]}]}

```

# Drawbacks
- `scale` does not support `years` and `month`.
In order to implement this, we need to handle leap years and other calendar features.