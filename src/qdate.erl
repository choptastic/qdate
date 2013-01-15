-module(qdate).

-export([
	to_string/1,
	to_string/2,
	to_date/1,
	to_now/1,
	to_unixtime/1,
	unixtime/0
]).

%% -export([
%% 	register_parser/2,
%% 	register_parser/1
%% ]).
%% 
%% -export([
%% 	set_timezone/1,
%% 	set_timezone/2,
%% 	get_timezone/0,
%% 	get_timezone/1
%% ]).


%% Exported for API compatibility with ec_date
-export([
	format/1,format/2,
	nparse/1,
	parse/1
]).

%% This the value in gregorian seconds for jan 1st 1970, 12am
%% It's used to convert to and from unixtime, since unixtime starts 
%% 1970-01-01 12:00am
-define(UNIXTIME_BASE, 62176219200).

to_string(Format) ->
	to_string(Format, now()).

to_string(Format, Date) ->
	ec_date:format(Format,to_date(Date)).

format(Format) ->
	to_string(Format).

format(Format, Date) ->
	to_string(Format, Date).

parse(String) ->
	to_date(String).

nparse(String) ->
	to_now(String).

to_date(Unixtime) when is_integer(Unixtime) ->
	unixtime_to_date(Unixtime);
to_date(DateString) when is_list(DateString) ->
	ec_date:parse(DateString);
to_date(Now = {_,_,_}) ->
	calendar:now_to_datetime(Now);
to_date(Date = {{_,_,_},{_,_,_}}) ->
	Date.


to_unixtime(Unixtime) when is_integer(Unixtime) ->
	Unixtime;
to_unixtime({MegaSecs,Secs,_}) ->
	MegaSecs*1000000 + Secs;
to_unixtime(ToParse) ->
	Date = to_date(ToParse),
	calendar:datetime_to_gregorian_seconds(Date) - ?UNIXTIME_BASE.

unixtime() ->
	to_unixtime(now()).

to_now(Now = {_,_,_}) ->
	Now;
to_now(ToParse) ->
	Unixtime = to_unixtime(ToParse),
	unixtime_to_now(Unixtime).


unixtime_to_now(T) when is_integer(T) ->
	MegaSec = floor(T/1000000),
	Secs = T - MegaSec*1000000,
	{MegaSec,Secs,0}.

unixtime_to_date(T) ->
	Now = unixtime_to_now(T),
	calendar:now_to_datetime(Now).

floor(N) when N >= 0 ->
	trunc(N);
floor(N) when N < 0 ->
	Int = trunc(N),
	if
		Int==N -> Int;
		true -> Int-1
	end.
		
