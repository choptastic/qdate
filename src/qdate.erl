% vim: ts=4 sw=4 et
% Copyright (c) 2013-2023 Jesse Gumm
% See LICENSE for licensing information.
%
-module(qdate).

%-compile(export_all).

-export([
    start/0,
    stop/0
]).

-export([
    to_string/1,
    to_string/2,
    to_string/3,
    to_string/4,
    to_date/1,
    to_date/2,
    to_date/3,
    to_now/1,
    to_now/2,
    to_unixtime/1,
    to_unixtime/2,
    unixtime/0
]).

-export([
    beginning_minute/1,
    beginning_minute/0,
    beginning_hour/1,
    beginning_hour/0,
    beginning_day/1,
    beginning_day/0,
    beginning_week/0,
    beginning_week/1,
    beginning_week/2,
    beginning_month/1,
    beginning_month/0,
    beginning_year/1,
    beginning_year/0
]).

-export([
     end_minute/1,
     end_minute/0,
     end_hour/1,
     end_hour/0,
     end_day/1,
     end_day/0,
     end_week/0,
     end_week/1,
     end_week/2,
     end_month/1,
     end_month/0,
     end_year/1,
     end_year/0
]).

-export([
    compare/2,
    compare/3,
    between/2,
    between/3,
    between/5
]).

-export([
    sort/1,
    sort/2,
    sort/3
]).

-export([
    add_seconds/2,
    add_seconds/1,
    add_minutes/2,
    add_minutes/1,
    add_hours/2,
    add_hours/1,
    add_days/2,
    add_days/1,
    add_weeks/2,
    add_weeks/1,
    add_months/2,
    add_months/1,
    add_years/2,
    add_years/1,
    add_unit/2,
    add_unit/3,
    add_date/2
]).

-export([
    range/4,
    range_seconds/3,
    range_minutes/3,
    range_hours/3,
    range_days/3,
    range_weeks/3,
    range_months/3,
    range_years/3
]).

-export([
    age/1,
    age/2,
    age_days/1,
    age_days/2
]).

-export([
    register_parser/2,
    register_parser/1,
    deregister_parser/1,
    deregister_parsers/0,
    get_parsers/0,

    register_format/2,
    deregister_format/1,
    get_formats/0,

    set_timezone/1,
    set_timezone/2,
    get_timezone/0,
    get_timezone/1,
    clear_timezone/0,
    clear_timezone/1
]).

-export([
    parse_relative/1
]).

%% Exported for API compatibility with ec_date
-export([
    format/1,format/2,
    nparse/1,
    parse/1
]).

-export([set_opt/2,
         get_opt/2,
         get_opts/0]).

-type qdate() :: any().
-type datetime() :: {{integer(), integer(), integer()}, {integer(), integer(), integer()}} |
                    {{integer(), integer(), integer()}, {integer(), integer(), integer(), integer()}}.
-type erlnow() :: {integer(), integer(), integer()}.
-type binary_or_string() :: binary() | string().
-type disambiguate() :: prefer_standard | prefer_daylight | both.

%% Default qdate opts. See qdate.config for more info
-define(QDATE_OPTS,
    [{default_timezone, "GMT"},
     {deterministic_parsing, {zero, zero}},
     {preserve_ms, false}]).

%% erlang:get_stacktrace/0 is deprecated in OTP 21
-ifndef(OTP_RELEASE).
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(), ).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-endif.

%% This the value in gregorian seconds for jan 1st 1970, 12am
%% It's used to convert to and from unixtime, since unixtime starts 
%% 1970-01-01 12:00am
-define(UNIXTIME_BASE,62167219200).


-define(DETERMINE_TZ, determine_timezone()).
-define(DEFAULT_DISAMBIG, prefer_standard).


start() ->
    application:ensure_all_started(qdate).

stop() ->
    ok.

to_string(Format) ->
    to_string(Format, os:timestamp()).

to_string(Format, Date) ->
    to_string(Format, ?DETERMINE_TZ, Date).

to_string(Format, ToTZ, Date) ->
    to_string(Format, ToTZ, ?DEFAULT_DISAMBIG, Date).

-spec to_string(Format :: any(), ToTZ :: any(), Disambiguate :: disambiguate(), Date :: qdate()) -> binary_or_string() | {ambiguous, binary_or_string() , binary_or_string()}.
to_string(FormatKey, ToTZ, Disambiguate, Date) when is_atom(FormatKey) orelse is_tuple(FormatKey) ->
    Format = case qdate_srv:get_format(FormatKey) of
        undefined -> throw({undefined_format_key,FormatKey});
        F -> F
    end,
    to_string(Format, ToTZ, Disambiguate, Date);
to_string(Format, ToTZ, Disambiguate, Date) when is_binary(Format) ->
    list_to_binary(to_string(binary_to_list(Format), ToTZ, Disambiguate, Date));
to_string(Format, ToTZ, Disambiguate, Date) when is_list(Format) ->
    %% it may seem odd that we're ensuring it here, and then again
    %% as one of the last steps of the to_date process, but we need
    %% the actual name for the strings for the PHP "T" and "e", so
    %% we extract the Timezone in case ToTZ is actually a timezone key
    %% Then we can pass it on to to_date as well. That way we don't have
    %% to do it twice, since it's already ensured.
    ActualToTZ = ensure_timezone(ToTZ),
    case to_date(ActualToTZ, Disambiguate, Date) of
        {ambiguous, Standard, Daylight} ->
            {ambiguous,
                to_string_worker(Format, ActualToTZ, prefer_standard, Standard),
                to_string_worker(Format, ActualToTZ, prefer_daylight, Daylight)};
        ActualDate ->
            case tz_name(ActualDate,Disambiguate, ActualToTZ) of
                {ambiguous,_,_} ->
                    {ambiguous,
                        to_string_worker(Format, ActualToTZ, prefer_standard, ActualDate),
                        to_string_worker(Format, ActualToTZ, prefer_daylight, ActualDate)};
                _ ->
                    to_string_worker(Format, ActualToTZ, Disambiguate, ActualDate)
            end
    end.
            
        

to_string_worker([], _, _, _) ->
    "";
to_string_worker([$\\,H|RestFormat], ToTZ, Disamb, Date) ->
    [H|to_string_worker(RestFormat, ToTZ, Disamb, Date)];
to_string_worker([$e|RestFormat], ToTZ, Disamb, Date) ->
    ToTZ ++ to_string_worker(RestFormat, ToTZ, Disamb, Date);
to_string_worker([$I|RestFormat], ToTZ, Disamb, Date) ->
    I = case localtime_dst:check(Date, ToTZ) of
        is_in_dst       -> "1";
        is_not_in_dst   -> "0";
        ambiguous_time  -> "?"
    end,
    I ++ to_string_worker(RestFormat, ToTZ, Disamb, Date);
to_string_worker([H | RestFormat], ToTZ, Disamb, Date) when H==$O orelse H==$P ->
    Shift = get_timezone_shift(ToTZ, Disamb, Date),
    Separator = case H of
        $O -> "";
        $P -> ":"
    end,
    format_shift(Shift,Separator) ++ to_string_worker(RestFormat, ToTZ, Disamb, Date);
to_string_worker([$T | RestFormat], ToTZ, Disamb, Date) ->
    ShortName = tz_name(Date, Disamb, ToTZ),
    ShortName ++ to_string_worker(RestFormat, ToTZ, Disamb, Date);
to_string_worker([$Z | RestFormat], ToTZ, Disamb, Date) ->
    {Sign, Hours, Mins} = get_timezone_shift(ToTZ, Disamb, Date),
    Seconds = (Hours * 3600) + (Mins * 60),
    atom_to_list(Sign)  ++ integer_to_list(Seconds) ++ to_string_worker(RestFormat, ToTZ, Disamb, Date);
to_string_worker([$r | RestFormat], ToTZ, Disamb, Date) ->
    NewFormat = "D, d M Y H:i:s O",
    to_string_worker(NewFormat, ToTZ, Disamb, Date) ++ to_string_worker(RestFormat, ToTZ, Disamb, Date);
to_string_worker([$c | RestFormat], ToTZ, Disamb, Date) ->
    Format1 = "Y-m-d",
    Format2 = case Date of
                  {_, {_,_,_,_}} ->
                      %% Have milliseconds
                      "H:i:s.fP";
                  _ ->
                      "H:i:sP"
              end,
    to_string_worker(Format1, ToTZ, Disamb, Date) 
            ++ "T" 
            ++ to_string_worker(Format2, ToTZ, Disamb, Date) 
            ++ to_string_worker(RestFormat, ToTZ, Disamb, Date);
to_string_worker([H | RestFormat], ToTZ, Disamb, Date) ->
    ec_date:format([H], Date) ++ to_string_worker(RestFormat, ToTZ, Disamb, Date).

tz_name(Date, Disambiguate, ToTZ) ->
    case localtime:tz_name(Date, ToTZ) of
        {ShortName, _} when is_list(ShortName) ->
            ShortName;
        {{ShortStandard,_},{ShortDST,_}} -> 
            case Disambiguate of
                prefer_standard -> ShortStandard;
                prefer_daylight -> ShortDST;
                both            -> {ambiguous, ShortStandard, ShortDST}
            end
    end.
        
format_shift({Sign,Hours,Mins},Separator) ->
    SignStr = atom_to_list(Sign),
    MinStr = leading_zero(Mins),
    HourStr = leading_zero(Hours),
    SignStr ++ HourStr ++ Separator ++ MinStr.

leading_zero(I) when I < 10 ->
    "0" ++ integer_to_list(I);
leading_zero(I) ->
    integer_to_list(I).


format(Format) ->
    to_string(Format).

format(Format, Date) ->
    to_string(Format, Date).

parse(String) ->
    to_date(String).

nparse(String) ->
    to_now(String).


to_date(RawDate) ->
    to_date(?DETERMINE_TZ, RawDate).

to_date(ToTZ, RawDate) ->
    to_date(ToTZ, ?DEFAULT_DISAMBIG, RawDate).

-spec to_date(ToTZ :: any(), Disambiguate :: disambiguate(), RawDate :: any()) -> {ambiguous, datetime(), datetime()} | datetime().
to_date(ToTZ, Disambiguate, RawDate) when is_binary(RawDate) ->
    to_date(ToTZ, Disambiguate, binary_to_list(RawDate));
to_date(ToTZ, Disambiguate, RawDate) when is_binary(ToTZ) ->
    to_date(binary_to_list(ToTZ), Disambiguate, RawDate);
to_date(ToTZ, Disambiguate, RawDate)  ->
    {ExtractedDate, ExtractedTZ} = extract_timezone(RawDate),
    {RawDate3, FromTZ} = case try_registered_parsers(RawDate) of
        undefined ->
            {ExtractedDate, ExtractedTZ};
        {ParsedDate,undefined} ->
            {ParsedDate,ExtractedTZ};
        {ParsedDate,ParsedTZ} ->
            {ParsedDate,ParsedTZ}
    end,
    PreserveMs = preserve_ms(),
    try raw_to_date(RawDate3) of
        D={{_,_,_},{_,_,_}} ->
            date_tz_to_tz(D, Disambiguate, FromTZ, ToTZ);
        {{Year, Month, Date},{Hour,Minute,Second,Millis}} when PreserveMs ->
            {ODate, {OHour,OMinute,OSecond}} = date_tz_to_tz({{Year, Month, Date},{Hour,Minute,Second}}, Disambiguate, FromTZ, ToTZ),
            {ODate, {OHour,OMinute,OSecond, Millis}};
        {{Year, Month, Date},{Hour,Minute,Second,_Millis}} ->
            date_tz_to_tz({{Year, Month, Date},{Hour,Minute,Second}}, Disambiguate, FromTZ, ToTZ)
    catch
        _:_ ->
            case raw_to_date(RawDate) of
                D2={{_,_,_},{_,_,_}} ->
                    date_tz_to_tz(D2, Disambiguate, ?DETERMINE_TZ, ToTZ)
            end
    end.

%% This converts dates without regard to timezone.
%% Unixtime just goes to UTC
raw_to_date(Unixtime) when is_integer(Unixtime) ->
    unixtime_to_date(Unixtime);
raw_to_date(DateString) when is_list(DateString) ->
    ec_date:parse(DateString, get_deterministic_datetime());
raw_to_date(Now = {_,_,_}) ->
    calendar:now_to_datetime(Now);
raw_to_date(Date = {{_,_,_},{_,_,_}}) ->
    Date.

get_deterministic_datetime() ->
    DateZero = {1970,1,1},
    TimeZero = {0,0,0},
    case get_opt(deterministic_parsing, undefined) of
        {zero, zero}  -> {DateZero, TimeZero};
        {zero, now}   -> {DateZero, time()};
        {now, zero}   -> {date(), TimeZero};
        {now, now}    -> {date(), time()};
        undefined     -> {DateZero, TimeZero};
        Val           -> throw({invalid_env_var, {qdate, deterministic_parsing, Val}})
    end.

to_unixtime(Date) ->
    to_unixtime(?DEFAULT_DISAMBIG, Date).

-spec to_unixtime(Disamb :: disambiguate(), qdate()) -> {ambiguous, integer(), integer()} | integer().
to_unixtime(_, Unixtime) when is_integer(Unixtime) ->
    Unixtime;
to_unixtime(_, {MegaSecs,Secs,_}) when is_integer(MegaSecs), is_integer(Secs) ->
    MegaSecs*1000000 + Secs;
to_unixtime(Disamb, ToParse) ->
    %% We want to treat all unixtimes as GMT
    case to_date("GMT", Disamb, ToParse) of
        {ambiguous, Standard, Daylight} ->
            {ambiguous,
                calendar:datetime_to_gregorian_seconds(Standard) - ?UNIXTIME_BASE,
                calendar:datetime_to_gregorian_seconds(Daylight) - ?UNIXTIME_BASE};
        Date ->
            calendar:datetime_to_gregorian_seconds(Date) - ?UNIXTIME_BASE
    end.

unixtime() ->
    to_unixtime(os:timestamp()).

to_now(Date) ->
    to_now(?DEFAULT_DISAMBIG, Date).

-spec to_now(Disamb :: disambiguate(), qdate()) -> erlnow() | {ambiguous, erlnow(), erlnow()}.
to_now(_, Now = {_,_,_}) ->
    Now;
to_now(Disamb, ToParse) ->
    case to_unixtime(Disamb, ToParse) of
        {ambiguous, Standard, Daylight} when is_integer(Standard), is_integer(Daylight) ->
            {ambiguous, 
                unixtime_to_now(Standard),
                unixtime_to_now(Daylight)};
        Unixtime ->
            unixtime_to_now(Unixtime)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Beginning/Truncation  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beginning_minute() ->
    beginning_minute({date(),time()}).

beginning_minute(Date) ->
    {{Y,M,D},{H,I,_}} = to_date(Date),
    {{Y,M,D},{H,I,0}}.

beginning_hour() ->
    beginning_hour({date(),time()}).

beginning_hour(Date) ->
    {{Y,M,D},{H,_,_}} = to_date(Date),
    {{Y,M,D},{H,0,0}}.

beginning_day() ->
    beginning_day(unixtime()).

beginning_day(Date) ->
    {{Y,M,D},{_,_,_}} = to_date(Date),
    {{Y,M,D},{0,0,0}}.

beginning_month() ->
    beginning_month({date(),time()}).

beginning_month(Date) ->
    {{Y,M,_},{_,_,_}} = to_date(Date),
    {{Y,M,1},{0,0,0}}.

beginning_year() ->
    beginning_year({date(),time()}).

beginning_year(Date) ->
    {{Y,_,_},{_,_,_}} = to_date(Date),
    {{Y,1,1},{0,0,0}}.

beginning_week() ->
    beginning_week({date(), time()}).

%% 1 = Monday, 7 = Sunday
beginning_week(Date) ->
    beginning_week(1, Date).

beginning_week(BeginningDayOfWeek, Date) when is_atom(BeginningDayOfWeek) ->
    DOW = weekday_map(BeginningDayOfWeek),
    beginning_week(DOW, Date);
beginning_week(BeginningDayOfWeek, Date0) when
        BeginningDayOfWeek >= 1,
        BeginningDayOfWeek =< 7,
        is_integer(BeginningDayOfWeek) ->
    {DateOnly, _} = Date = to_date(Date0),
    CurDOW = calendar:day_of_the_week(DateOnly),
    if
        CurDOW==BeginningDayOfWeek ->
            {DateOnly, {0,0,0}};
        CurDOW > BeginningDayOfWeek->
            Diff = CurDOW - BeginningDayOfWeek,
            beginning_day(add_days(-Diff, Date));
        CurDOW < BeginningDayOfWeek ->
            Diff = 7 - (BeginningDayOfWeek - CurDOW),
            beginning_day(add_days(-Diff, Date))
    end.

weekday_map(mon) -> 1;
weekday_map(tue) -> 2;
weekday_map(wed) -> 3;
weekday_map(thu) -> 4;
weekday_map(fri) -> 5;
weekday_map(sat) -> 6;
weekday_map(sun) -> 7;

weekday_map(monday) -> 1;
weekday_map(tuesday) -> 2;
weekday_map(wednesday) -> 3;
weekday_map(thursday) -> 4;
weekday_map(friday) -> 5;
weekday_map(saturday) -> 6;
weekday_map(sunday) -> 7.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% End of Period (day/hour, etc)  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


end_minute() ->
    end_minute({date(),time()}).

end_minute(Date) ->
    {{Y,M,D},{H,I,_}} = to_date(Date),
    {{Y,M,D},{H,I,59}}.

end_hour() ->
    end_hour({date(),time()}).

end_hour(Date) ->
    {{Y,M,D},{H,_,_}} = to_date(Date),
    {{Y,M,D},{H,59,59}}.

end_day() ->
    end_day({date(),time()}).

end_day(Date) ->
    {{Y,M,D},_} = to_date(Date),
    {{Y,M,D},{23,59,59}}.

end_month() ->
    end_month({date(), time()}).

end_month(Date) ->
    Beginning = beginning_month(Date),
    add_seconds(-1, add_months(1, Beginning)).

end_year() ->
    end_year({date(),time()}).

end_year(Date) ->
    {{Y,_,_},_} = to_date(Date),
    {{Y,12,31},{23,59,59}}.

end_week() ->
    end_week({date(), time()}).

end_week(Date) ->
    end_week(1, Date).

end_week(BeginningDayOfWeek, Date) ->
    Beginning = beginning_week(BeginningDayOfWeek, Date),
    PlusWeek = add_weeks(1, Beginning),
    add_seconds(-1, PlusWeek).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Comparisons     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compare(A :: qdate(), B :: qdate()) -> integer().
compare(A, B) ->
    NowA = to_now(A),
    NowB = to_now(B),
    if
        NowA == NowB -> 0;
        NowA < NowB -> -1;
        NowA > NowB -> 1
    end.

-spec compare(A :: qdate(), Op :: atom(), B :: qdate()) -> boolean().
compare(A, Op, B) ->
    Comp = compare(A, B),
    case Op of
        '=='    -> Comp =:= 0;
        '='     -> Comp =:= 0;

        '!='    -> Comp =/= 0;
        '=/='   -> Comp =/= 0;
        '/='    -> Comp =/= 0;

        'before'-> Comp =:= -1;
        '<'     -> Comp =:= -1;
        '<='    -> Comp =:= -1 orelse Comp =:= 0;
        '=<'    -> Comp =:= -1 orelse Comp =:= 0;

        'after' -> Comp =:= 1;
        '>'     -> Comp =:= 1;
        '>='    -> Comp =:= 1 orelse Comp =:= 0;
        '=>'    -> Comp =:= 1 orelse Comp =:= 0
    end.

between(A, B) ->
    between(A, unixtime(), B).

between(A, Date, B) ->
    between(A, '=<', Date, '=<', B).

between(A, Op1, Date, Op2, B) ->
    compare(A, Op1, Date) andalso compare(Date, Op2, B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Sorting    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort(List) ->
    sort('=<', List).

sort(Op, List) ->
    sort(Op, List, [{non_dates, back}]).

sort(Op, List, Opts) ->
    NonDateOpt = proplists:get_value(non_dates, Opts, back),
    WithNorm = add_sort_normalization(List, NonDateOpt),
    SortFun = make_sort_fun(Op, NonDateOpt),
    Sorted = lists:sort(SortFun, WithNorm),
    strip_sort_normalization(Sorted).

%% Normalization pre-processes the dates (converting them to unixtimes for easy
%% comparison, and also tags non-dates (dates that crashed during parsing) as such
add_sort_normalization(List, NonDateOpt) ->
    lists:map(fun(Date) ->
        Sortable = try to_unixtime(Date)
                   catch _:_ when NonDateOpt=/=crash ->
                        {non_date, Date}
                   end,
        {Sortable, Date}
    end, List).

%% Remove the normalization tag to return the original term
strip_sort_normalization(List) ->
    [Date || {_, Date} <- List].

-spec make_sort_fun(Op :: atom(), NonDateOpt :: front | back) -> fun().
make_sort_fun(Op, NonDateOpt) ->
    DateComp = sort_op_comp_fun(Op),
    
    fun({{non_date, A}, _}, {{non_date, B},_}) ->
            DateComp(A,B);
       ({{non_date, _}, _}, _) when NonDateOpt == front ->
            true;
       ({{non_date, _}, _}, _) when NonDateOpt == back ->
            false;
       (_, {{non_date, _}, _}) when NonDateOpt == front ->
            false;
       (_, {{non_date, _}, _}) when NonDateOpt == back ->
            true;
       (A, B) ->
            DateComp(A, B)
    end.
   
sort_op_comp_fun(Op) ->
    fun(A, B) ->
        case Op of
            'before'-> A < B;
            '<'     -> A < B;
            '<='    -> A =< B;
            '=<'    -> A =< B;

            'after' -> A > B;
            '>'     -> A > B;
            '>='    -> A >= B;
            '=>'    -> A >= B
        end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Date Math       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_seconds(Seconds, Date) ->
    to_unixtime(Date) + Seconds.

add_seconds(Seconds) ->
    add_seconds(Seconds, os:timestamp()).

add_minutes(Minutes, Date) ->
    add_seconds(Minutes * 60, Date).

add_minutes(Minutes) ->
    add_minutes(Minutes, os:timestamp()).

add_hours(Hours, Date) ->
    add_seconds(Hours * 3600, Date).

add_hours(Hours) ->
    add_hours(Hours, os:timestamp()).

add_days(Days, Date0) ->
    {{Y,M,D},Time} = to_date(Date0),
    to_unixtime(fix_maybe_improper_date({{Y, M, D+Days}, Time})). 

add_days(Days) ->
    add_days(Days, os:timestamp()).

add_weeks(Weeks, Date) ->
    add_days(Weeks * 7, Date).

add_weeks(Weeks) ->
    add_weeks(Weeks, os:timestamp()).

add_months(Months, Date) ->
    {{Y,M,D}, Time} = to_date(Date),
    {TargetYear, TargetMonth} = fix_year_month({Y,M+Months}),
    DaysInMonth = calendar:last_day_of_the_month(TargetYear, TargetMonth),
    NewD = lists:min([DaysInMonth, D]),
    to_unixtime(fix_maybe_improper_date({{Y, M+Months, NewD}, Time})).

add_months(Months) ->
    add_months(Months, os:timestamp()).

add_years(Years, Date) ->
    {{Y,M,D}, Time} = to_date(Date),
    TargetYear = Y+Years,
    NewD = case M of
        2 -> 
            DaysInMonth = calendar:last_day_of_the_month(TargetYear, M),
            lists:min([DaysInMonth, D]);
        _ ->
            D
    end,
    to_unixtime({{Y+Years, M, NewD}, Time}).

add_years(Years) ->
    add_years(Years, os:timestamp()).

-type unit() :: second | seconds |
                minute | minutes |
                hour | hours |
                day | days |
                week | weeks |
                month | months |
                year | years.

-spec add_unit(Unit :: unit(), Value :: integer(), Date :: qdate()) -> qdate().
add_unit(second, Value, Date) ->
    add_unit(seconds, Value, Date);
add_unit(seconds, Value, Date) ->
    add_seconds(Value, Date);
add_unit(minute, Value, Date) ->
    add_unit(minutes, Value, Date);
add_unit(minutes, Value, Date) ->
    add_minutes(Value, Date);
add_unit(hour, Value, Date) ->
    add_unit(hours, Value, Date);
add_unit(hours, Value, Date) ->
    add_hours(Value, Date);
add_unit(day, Value, Date) ->
    add_unit(days, Value, Date);
add_unit(days, Value, Date) ->
    add_days(Value, Date);
add_unit(week, Value, Date) ->
    add_unit(weeks, Value, Date);
add_unit(weeks, Value, Date) ->
    add_weeks(Value, Date);
add_unit(month, Value, Date) ->
    add_unit(months, Value, Date);
add_unit(months, Value, Date) ->
    add_months(Value, Date);
add_unit(year, Value, Date) ->
    add_unit(years, Value, Date);
add_unit(years, Value, Date) ->
    add_years(Value, Date).

add_unit(Unit, Value) ->
    add_unit(Unit, Value, os:timestamp()).


add_date({{AddY, AddM, AddD}, {AddH, AddI, AddS}}, Date) ->
    {{Y, M, D}, {H, I, S}} = to_date(Date),
    Date1 = fix_maybe_improper_date({{Y+AddY, M+AddM, D+AddD}, {H, I, S}}),
    Date2 = to_unixtime(Date1),
    Date2 + AddS + (AddI*60) + (AddH*3600).


-define(IS_LEAP_YEAR(Y), (Y rem 4 =:= 0 andalso
                             (Y rem 100 =/= 0
                              orelse Y rem 400 =:= 0))).

fix_maybe_improper_date({Date0, Time}) ->
    Date = fmid(Date0),
    {Date, Time}.


%% Originally, this function didn't recurse.  Here's the story.  Some numbers,
%% like M = 12 (December) or M = -11 (January) would trigger an overflow or
%% underflow, resulting in fix_year_month returning something nonsensical like
%% {2018, 13}.  I added some extra clauses to special treat those "overflow but
%% shouldn't" situations, but realized it was just cleaner to recurse, calling
%% fix_year_month on the calculated result, knowing that the numbers will
%% normalize on their own. So for all the clauses of fix_year_month, we recurse
%% as a sanity check, eventually only returning the result of the "Everything
%% Looks good" clause at the bottom.
fix_year_month({Y, M}) when M > 12 ->
    YearsOver = M div 12,
    fix_year_month({Y + YearsOver, M-(YearsOver*12)});
fix_year_month({Y, M}) when M < 1 ->
    YearsUnder = (abs(M-1) div 12) + 1,
    fix_year_month({Y - YearsUnder, M+(YearsUnder*12)});
fix_year_month({Y, M}) ->
    {Y, M}.
    

fmid({Y, M, D}) when M > 12;
                     M < 1 ->
    {NewY, NewM} = fix_year_month({Y, M}),
    fmid({NewY, NewM, D});

fmid({Y, M, D}) when (D > 30 andalso (
                        M=:=4 orelse 
                        M=:=6 orelse
                        M=:=9 orelse
                        M=:=11)) ->
    fmid({Y, M+1, D-30});
fmid({Y, M, D}) when M=:=2 andalso D > 29 andalso ?IS_LEAP_YEAR(Y) ->
    fmid({Y, M+1, D-29});
fmid({Y, M, D}) when M =:= 2 andalso D > 28 andalso not(?IS_LEAP_YEAR(Y)) ->
    fmid({Y, M+1, D-28});
fmid({Y, M, D}) when D > 31 ->
    fmid({Y, M+1, D-31});

fmid({Y, M, D}) when D < 1 ->
    TargetMonth = case M-1 of
        0 -> 12;
        X -> X
    end,
    DaysInTargetMonth = calendar:last_day_of_the_month(Y, TargetMonth),
    fmid({Y, M-1, D+DaysInTargetMonth});


fmid(Date) ->
    Date.

age(Birth) ->
    age(Birth, os:timestamp()).

age(Birth, Now) ->
    %% B=Birth
    {{BY, BM, BD}, _} = to_date(Birth),
    %% C=Current
    {{CY, CM, CD}, _} = to_date(Now),
    if
        (CM > BM);
        (CM == BM andalso CD >= BD)
            -> CY - BY;
        true ->
            (CY - BY) - 1
    end.

age_days(Birth) ->
    age_days(Birth, os:timestamp()).

age_days(Birth, Now) ->
    case {to_date(Birth), to_date(Now)} of
        {{SameDay, _}, {SameDay, _}} ->
            0;
        {{BirthDate, BirthTime}, {NowDate, NowTime}} ->
            BirthDays = calendar:date_to_gregorian_days(BirthDate),
            NowDays = calendar:date_to_gregorian_days(NowDate),
            DiffDays = NowDays - BirthDays,
            if
                NowTime >= BirthTime ->
                    DiffDays;
                true ->
                    DiffDays-1
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      Ranges        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range(seconds, Interval, Start, Finish) ->
    range_inner(fun add_seconds/2, Interval, Start, Finish);
range(minutes, Interval, Start, Finish) ->
    range_inner(fun add_minutes/2, Interval, Start, Finish);
range(hours, Interval, Start, Finish) ->
    range_inner(fun add_hours/2, Interval, Start, Finish);
range(days, Interval, Start, Finish) ->
    range_inner(fun add_days/2, Interval, Start, Finish);
range(weeks, Interval, Start, Finish) ->
    range_inner(fun add_weeks/2, Interval, Start, Finish);
range(months, Interval, Start, Finish) ->
    range_inner(fun add_months/2, Interval, Start, Finish);
range(years, Interval, Start, Finish) ->
    range_inner(fun add_years/2, Interval, Start, Finish).

range_inner(IntervalFun, Interval, Start, Finish) when Interval > 0 ->
    %% If Interval>0, then we're ascending, and we want to compare start/end
    %% dates normally
    CompareFun = fun(S, F) -> compare(S, F) end,
    range_normalizer(IntervalFun, Interval, CompareFun, Start, Finish);
range_inner(IntervalFun, Interval, Start, Finish) when Interval < 0 ->
    %% If Interval<0, then we're descending, and we want to compare start/end
    %% dates backwards (we want to end when the Start Date is Lower than
    %% Finish)
    CompareFun = fun(S, F) -> compare(F, S) end,
    range_normalizer(IntervalFun, Interval, CompareFun, Start, Finish);
range_inner(_, Interval, _, _) when Interval==0 ->
    throw(interval_cannot_be_zero).

range_normalizer(IntervalFun, Interval, CompareFun, Start0, Finish0) ->
    %% Convert dates to unixtime for speed's sake
    Start = to_unixtime(Start0),
    Finish = to_unixtime(Finish0),
    %% Prepare the incrementer, so we just need to pass the date to the incrementer.
    Incrementer = fun(D) -> IntervalFun(Interval, D) end,
    range_worker(Incrementer, CompareFun, Start, Finish).

range_worker(Incrementer, CompareFun, Start, Finish) ->
    case CompareFun(Start, Finish) of
        0 -> [Finish]; %% Equal, so we add our Finish value
        1 -> []; %% Start is after Finish, so we add nothing
        -1 -> %% Start is before Finish, so we include it, and recurse
            NextDay = Incrementer(Start),
            [Start | range_worker(Incrementer, CompareFun, NextDay, Finish)]
    end.

range_seconds(Interval, Start, Finish) ->
    range(seconds, Interval, Start, Finish).

range_minutes(Interval, Start, Finish) ->
    range(minutes, Interval, Start, Finish).

range_hours(Interval, Start, Finish) ->
    range(hours, Interval, Start, Finish).

range_days(Interval, Start, Finish) ->
    range(days, Interval, Start, Finish).

range_weeks(Interval, Start, Finish) ->
    range(weeks, Interval, Start, Finish).

range_months(Interval, Start, Finish) ->
    range(months, Interval, Start, Finish).

range_years(Interval, Start, Finish) ->
    range(years, Interval, Start, Finish).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%  Relative Date Parsing  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_relative({relative, Date, Relation}) when is_atom(Relation) ->
    parse_relative({relative, Date, atom_to_list(Relation)});
parse_relative({relative, Date, Relation}) when is_list(Relation); is_binary(Relation) ->
    case parse_actual_relation(Relation) of
        undefined -> undefined;
        {OpStr, NumStr, UnitStr} ->
	    {Num, Unit} = normalize_relative_matches(OpStr, NumStr, UnitStr),
	    add_unit(Unit, Num, Date)
    end;
parse_relative(now) ->
    unixtime();
parse_relative("now") ->
    unixtime();
parse_relative(<<"now">>) ->
    unixtime();
parse_relative(Relation) when is_list(Relation); is_binary(Relation) ->
    parse_relative({relative, unixtime(), Relation});
parse_relative(_) ->
    undefined.


%% I would do this function recursively, but the return order of arguments
%% inconsistent, so I just leave it like this. It's a little nasty to have the
%% nested case expressions, but I can deal with it.
parse_actual_relation(Relation) ->
    PrefixRE = "^(\\-|\\+|in)\\s?(\\d+) (second|minute|hour|day|week|month|year)s?$",
    SuffixRE = "^(\\d+) (second|minute|hour|day|week|month|year)s?\\s?(ago|from now)?$",
    case re:run(Relation, PrefixRE, [{capture, all_but_first, list}]) of
        nomatch ->
            case re:run(Relation, SuffixRE, [{capture, all_but_first, list}]) of
                nomatch -> undefined;
                {match, [NumStr, UnitStr, OpStr]} ->
                    {OpStr, NumStr, UnitStr}
            end;
        {match, [OpStr, NumStr, UnitStr]} ->
            {OpStr, NumStr, UnitStr}
    end.

normalize_relative_matches(OpStr, NumStr, UnitStr) ->
    Op = normalize_relative_op(OpStr),
    Num = list_to_integer(Op ++ NumStr),
    Unit = list_to_existing_atom(UnitStr),
    {Num, Unit}.

normalize_relative_op(Op) ->
    case Op of
        "+"         -> "+";
        "-"         -> "-";
        "ago"       -> "-";
        "from now"  -> "+";
        "in"        -> "+"
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Timezone Stuff   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_timezone_shift(TZ, Disambiguate, Date) ->
    case localtime:tz_shift(Date, TZ) of
        unable_to_detect -> {error,unable_to_detect};
        {error,T} -> {error,T};
        {Sh, _} when Disambiguate==prefer_standard -> Sh;
        {_, Sh} when Disambiguate==prefer_daylight -> Sh;
        0 -> {'+', 0, 0};
        Sh -> Sh
    end.


extract_timezone(Unixtime) when is_integer(Unixtime) ->
    {Unixtime, "GMT"};
extract_timezone(DateString) when is_list(DateString) ->
    case extract_gmt_relative_timezone(DateString) of
        undefined -> 
            AllTimezones = localtime:list_timezones(),
            RevDate = lists:reverse(DateString),
            extract_timezone_helper(RevDate, AllTimezones);
        {Date, GMTRel} ->
            {Date, GMTRel}
    end;
extract_timezone(Date={{_,_,_},{_,_,_}}) ->
    {Date, ?DETERMINE_TZ};
extract_timezone(Rel={relative, _, _}) ->
    {Rel, "GMT"};
extract_timezone(Now={_,_,_}) ->
    {Now, "GMT"};
extract_timezone({MiscDate,TZ}) ->
    {MiscDate,TZ}.
    
extract_gmt_relative_timezone(DateString) ->
    RE = "^(.*?)(?:GMT|UTC)?([+-])(\\d{1,2}):?(\\d{2})?$",
    case re:run(DateString,RE,[{capture,all_but_first,list},caseless]) of
        {match, [NewDateStr, Sign, HourStr, MinStr]} ->
            {NewDateStr, minutes_from_gmt_relative_timezone(Sign, HourStr, MinStr)};
        {match, [NewDateStr, Sign, HourStr]} ->
            {NewDateStr, minutes_from_gmt_relative_timezone(Sign, HourStr, "0")};
        nomatch ->
            undefined
    end.

%% The number of minutes a the timezone is behind gmt
minutes_from_gmt_relative_timezone("+", HourStr, MinStr) ->
    -minutes_from_gmt_relative_timezone("-", HourStr, MinStr);
minutes_from_gmt_relative_timezone("-", HourStr, MinStr) ->
    list_to_integer(HourStr)*60 + list_to_integer(MinStr).

extract_timezone_helper(RevDate, []) ->
    {lists:reverse(RevDate), ?DETERMINE_TZ};
extract_timezone_helper(RevDate, [TZ | TZs]) when length(RevDate) >= length(TZ) ->
    RevTZ = lists:reverse(TZ),
    case lists:split(length(TZ),RevDate) of
        {RevTZ," " ++ Remainder} ->
            {lists:reverse(Remainder), TZ};
        _ ->
            extract_timezone_helper(RevDate, TZs)
    end;
extract_timezone_helper(RevDate, [_TZ | TZs]) ->
    extract_timezone_helper(RevDate, TZs).

preserve_ms() ->
    get_opt(preserve_ms, false).

%% This is the timezone only if the qdate application variable 
%% "default_timezone" isn't set or is set to undefined.
%% It's recommended that your app sets the var in a config, or at least using
%%
%%      qdate:set_opt(default_timezone, "GMT").
%%
default_timezone() ->
    case get_opt(default_timezone, undefined) of
        undefined -> "GMT";
        {Mod, Fun} -> Mod:Fun();
        TZ -> TZ
    end.

determine_timezone() ->
    case qdate_srv:get_timezone() of
        undefined -> default_timezone();
        TZ -> TZ
    end.

%% If FromTZ is an integer, then it's an integer that represents the number of minutes
%% relative to GMT. So we convert the date to GMT based on that number, then we can
%% do the other timezone conversion.
-spec date_tz_to_tz(Date :: datetime(), Disambiguate :: disambiguate(), FromTZ :: any(), ToTZ :: any()) ->
    datetime() | {ambiguous, datetime(), datetime()}.
date_tz_to_tz(Date, Disambiguate, FromTZ, ToTZ) when is_integer(FromTZ) ->
    NewDate = localtime:adjust_datetime(Date, FromTZ),
    date_tz_to_tz(NewDate, Disambiguate, "GMT", ToTZ);
date_tz_to_tz(Date, Disambiguate, FromTZ, ToTZ) ->
    ActualToTZ = ensure_timezone(ToTZ), 
    case Disambiguate of
        prefer_standard ->
            localtime:local_to_local(Date, FromTZ, ActualToTZ);
        prefer_daylight ->
            localtime:local_to_local_dst(Date, FromTZ, ActualToTZ);
        both ->
            date_tz_to_tz_both(Date, FromTZ, ToTZ)
    end.

-spec date_tz_to_tz_both(Date :: datetime(), FromTZ :: string(), ToTZ :: string()) -> datetime() | {ambiguous, datetime(), datetime()}.
date_tz_to_tz_both(Date, FromTZ, ToTZ) ->
    Standard = localtime:local_to_local(Date, FromTZ, ToTZ),
    Daylight = localtime:local_to_local_dst(Date, FromTZ, ToTZ),
    if
        Standard=:=Daylight ->
            Standard;
        true ->
            {ambiguous, Standard, Daylight}
    end.           

set_timezone(TZ) when is_binary(TZ) ->
    set_timezone(binary_to_list(TZ));
set_timezone(TZ) ->
    qdate_srv:set_timezone(TZ).


set_timezone(Key,TZ) when is_binary(TZ) ->
    set_timezone(Key, binary_to_list(TZ));
set_timezone(Key,TZ) ->
    qdate_srv:set_timezone(Key, TZ).

get_timezone() ->
    ?DETERMINE_TZ.

get_timezone(Key) ->
    qdate_srv:get_timezone(Key).

ensure_timezone(auto) ->
    ?DETERMINE_TZ;
ensure_timezone(Key) when is_atom(Key) orelse is_tuple(Key) ->
    case get_timezone(Key) of
        undefined -> throw({timezone_key_not_found,Key});
        ToTZ -> ToTZ
    end;
ensure_timezone(TZ) when is_binary(TZ) ->
    binary_to_list(TZ);
ensure_timezone(TZ) when is_list(TZ) ->
    TZ.

clear_timezone() ->
    qdate_srv:clear_timezone().

clear_timezone(Key) ->
    qdate_srv:clear_timezone(Key).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Register Parsers  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_parsers() ->
    qdate_srv:get_parsers().

register_parser(Key, Parser) when is_function(Parser,1) ->
    qdate_srv:register_parser(Key,Parser).

register_parser(Parser) when is_function(Parser,1) ->
    qdate_srv:register_parser(Parser).

deregister_parser(Key) ->
    qdate_srv:deregister_parser(Key).

deregister_parsers() ->
    qdate_srv:deregister_parsers().

try_registered_parsers(RawDate) ->
    Parsers = qdate_srv:get_parsers(),
    try_parsers(RawDate,Parsers).
    
try_parsers(_RawDate,[]) ->
    undefined;
try_parsers(RawDate,[{ParserKey,Parser}|Parsers]) ->
    try Parser(RawDate) of
        Timestamp when is_integer(Timestamp) ->
            {Timestamp, "GMT"};
        {{_,_,_},{_,_,_}} = DateTime ->
            {DateTime,undefined};
        {DateTime={{_,_,_},{_,_,_}},Timezone} ->
            {DateTime,Timezone};
        undefined ->
            try_parsers(RawDate, Parsers);
        Other ->
            throw({invalid_parser_return_value,[{parser_key,ParserKey},{return,Other}]})
    catch
        ?WITH_STACKTRACE(Error, Reason, Stacktrace)
            throw({error_in_parser,[{error,{Error,Reason}},{parser_key,ParserKey}, {stacktrace, Stacktrace}]})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Register Formats  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_formats() ->
    qdate_srv:get_formats().

register_format(Key, Format) ->
    qdate_srv:register_format(Key, Format).

deregister_format(Key) ->
    qdate_srv:deregister_format(Key).



unixtime_to_now(T) when is_integer(T) ->
    MegaSec = flooring(T/1000000),
    Secs = T - MegaSec*1000000,
    {MegaSec,Secs,0}.

unixtime_to_date(T) ->
    Now = unixtime_to_now(T),
    calendar:now_to_datetime(Now).

flooring(N) when N >= 0 ->
    erlang:trunc(N);
flooring(N) when N < 0 ->
    Int = erlang:trunc(N),
    if
        Int==N -> Int;
        true -> Int-1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  OPTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_opt(Key, Val) ->
    {Key, _} = lists:keyfind(Key, 1, ?QDATE_OPTS),
    persistent_term:put({qdate, Key}, Val).

get_opt(Key, Default) ->
    persistent_term:get({qdate, Key}, Default).

get_opts() ->
    [{Opt, persistent_term:get({qdate, Opt}, undefined)} || {Opt, _} <- ?QDATE_OPTS].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%% emulates as if a forum-type website has a Site tz, and a user-specified tz
-define(SITE_TZ,"PST").
-define(USER_TZ,<<"CST">>).
-define(SELF_TZ,"EST"). %% Self will be the pid of the current running process
-define(SITE_KEY,test_site_key).
-define(USER_KEY,test_user_key).

tz_test_() ->
    {
        setup,
        fun start_test/0,
        fun stop_test/1,
        fun(SetupData) ->
            {inorder,[
                simple_test(SetupData),
                compare_test(SetupData),
                tz_tests(SetupData),
                parser_format_test(SetupData),
                test_deterministic_parser(SetupData),
                test_disambiguation(SetupData),
                arith_tests(SetupData)
            ]}
        end
    }.

tz_preserve_ms_true_test_() ->
    {
        setup,
        fun start_test/0,
        fun stop_test/1,
        fun(SetupData) ->
            {inorder,[
                preserve_ms_true_tests(SetupData)
            ]}
        end
    }.

tz_preserve_ms_false_test_() ->
    {
        setup,
        fun start_test/0,
        fun stop_test/1,
        fun(SetupData) ->
            {inorder,[
                preserve_ms_false_tests(SetupData)
            ]}
        end
    }.

test_deterministic_parser(_) ->
    {inorder, [
        ?_assertEqual(ok, set_opt(deterministic_parsing, {now, now})),
        ?_assertEqual({date(), {7,0,0}}, qdate:to_date("7am")),
        ?_assertEqual({{2012,5,10}, time()}, qdate:to_date("2012-5-10")),

        ?_assertEqual(ok, set_opt(deterministic_parsing, {zero, zero})),
        ?_assertEqual({{1970,1,1}, {7,0,0}}, qdate:to_date("7am")),
        ?_assertEqual({{2012,5,10}, {0,0,0}}, qdate:to_date("2012-5-10")),

        ?_assertEqual(ok, set_opt(deterministic_parsing, undefined)),
        ?_assertEqual({{1970,1,1}, {7,0,0}}, qdate:to_date("7am")),
        ?_assertEqual({{2012,5,10}, {0,0,0}}, qdate:to_date("2012-5-10"))
    ]}.

test_disambiguation(_) ->
    {inorder, [
        ?_assertEqual(ok, set_timezone("America/New York")),
        ?_assertEqual({ambiguous, {{2013,11,3},{6,0,0}}, {{2013,11,3},{5,0,0}}}, qdate:to_date("GMT",both,{{2013,11,3},{1,0,0}})),
        ?_assertEqual({{2013,11,3},{6,0,0}}, qdate:to_date("GMT",prefer_standard,{{2013,11,3},{1,0,0}})),
        ?_assertEqual({{2013,11,3},{5,0,0}}, qdate:to_date("GMT",prefer_daylight,{{2013,11,3},{1,0,0}})),
        ?_assertEqual({ambiguous, "GMT","GMT"}, qdate:to_string("T", "GMT", both, {{2013,11,3},{1,0,0}})),
        ?_assertEqual({ambiguous, "EST","EDT"}, qdate:to_string("T", auto, both, {{2013,11,3},{1,0,0}})),
        ?_assertEqual(ok, set_timezone("GMT")),
        ?_assertEqual({ambiguous, {{2013,11,3},{1,0,0}}, {{2013,11,3},{2,0,0}}}, qdate:to_date("America/New York", both, {{2013,11,3},{6,0,0}})),
        ?_assertEqual({{2013,11,3},{2,0,0}}, qdate:to_date("America/New York", prefer_daylight, {{2013,11,3},{6,0,0}})),
        ?_assertEqual({{2013,11,3},{1,0,0}}, qdate:to_date("America/New York", prefer_standard, {{2013,11,3},{6,0,0}}))
   ]}.


tz_tests(_) ->
    {inorder,[
        ?_assertEqual(ok,set_timezone(<<"Europe/Moscow">>)),
        ?_assertEqual("Europe/Moscow", get_timezone()),
        ?_assertEqual(ok,set_timezone(?SELF_TZ)),
        ?_assertEqual(?SELF_TZ,get_timezone()),
        ?_assertEqual("CST",get_timezone(?USER_KEY)),
        ?_assertEqual(?SITE_TZ,get_timezone(?SITE_KEY)),
        ?_assertEqual({{2013,3,7},{0,0,0}}, to_date(?USER_KEY,"3/7/2013 1:00am EST")),
        ?_assertEqual({{2013,3,7},{0,0,0}}, to_date(?SITE_KEY,"3/7/2013 3:00am EST")),
        ?_assertEqual({{2013,3,7},{2,0,0}}, to_date("3/7/2013 1:00am CST")), %% will use the current pid's setting
        ?_assertEqual("America/Chicago",to_string("e","America/Chicago","3/7/2013 1:00am")),
        ?_assertEqual("-0500",to_string("O","EST","3/7/2013 1:00am CST")),
        ?_assertEqual("-05:00",to_string("P","EST","3/7/2013 1:00am CST")),
        ?_assertEqual("EST",to_string("T","America/New York","3/7/2013 1:00am CST")),
        ?_assertEqual(?SITE_TZ,to_string("T",?SITE_KEY,"3/7/2013 1:00am CST")),
        ?_assertEqual(integer_to_list(-5 * 3600), to_string("Z","EST","3/7/2013 1:00am CST")),
        ?_assertEqual("Thu, 07 Mar 2013 13:15:00 -0500", to_string("r","EST", "3/7/2013 1:15:00pm")),
        ?_assertEqual("2013-03-07T13:15:00-05:00", to_string("c", "EST", "3/7/2013 1:15:00pm")),

        ?_assertEqual({{2013,3,7},{6,0,0}}, to_date("GMT","3/7/2013 12:00am -0600")),
        ?_assertEqual({{2013,3,7},{6,0,0}}, to_date("GMT","3/7/2013 12:00am -600")),
        ?_assertEqual({{2013,3,7},{6,0,0}}, to_date("GMT","3/7/2013 12:00am GMT-0600")),
        ?_assertEqual({{2013,3,7},{6,0,0}}, to_date("GMT","3/7/2013 12:00am utc-0600")),
        ?_assertEqual({{2013,3,7},{1,0,0}}, to_date("EST","3/7/2013 12:00am utc-0600")),
        ?_assertEqual({{2013,3,6},{18,0,0}}, to_date("GMT","3/7/2013 12:00am +0600")),
        ?_assertEqual({{2013,3,6},{12,0,0}}, to_date("CST","3/7/2013 12:00am +0600")),

        %% These next two test check to make sure that the tz database properly
        %% interprets GMT+/-X timezones (an earlier issue with
        %% erlang_localtime's tz database had it incrementing/decrementing the
        %% minute field rather than hours.
        %%
        %% It also ensures that GMT+/-X handling is interpreted the way you'd
        %% intuitively expect, rather than the POSIX way, which is, quite
        %% frankly, broken.
        ?_assertEqual({{2013,3,7},{10,0,0}}, to_date("GMT-0","3/7/2013 10:00am GMT")),
        ?_assertEqual({{2013,3,7},{10,0,0}}, to_date("GMT+0","3/7/2013 10:00am GMT")),
        ?_assertEqual({{2013,3,7},{9,0,0}}, to_date("GMT-1","3/7/2013 10:00am GMT")),
        ?_assertEqual({{2013,3,7},{11,0,0}}, to_date("GMT+1","3/7/2013 10:00am GMT")),


        %% parsing, then reformatting the same time with a different timezone using the php "r" (rfc2822)
        ?_assertEqual("Thu, 07 Mar 2013 12:15:00 -0600",
            to_string("r","CST",to_string("r","EST",{{2013,3,7},{13,15,0}}))),

        %% A bunch of unixtime and now tests with timezones
        ?_assertEqual("1987-08-10 00:59:15 GMT",to_string("Y-m-d H:i:s T","GMT",555555555)),
        ?_assertEqual("1987-08-09 19:59:15 CDT",to_string("Y-m-d H:i:s T","CDT",555555555)),
        ?_assertEqual("1987-08-09 20:59:15 EDT",to_string("Y-m-d H:i:s T","America/New York",555555555)),
        ?_assertEqual(ok, set_timezone("GMT")),
        ?_assertEqual(555555555,to_unixtime("1987-08-10 00:59:15 GMT")),
        ?_assertEqual({555,555555,0},to_now("1987-08-10 00:59:15 GMT")),
        ?_assertEqual(ok, set_timezone("EST")),
        ?_assertEqual(555555555,to_unixtime("1987-08-10 00:59:15 GMT")),
        ?_assertEqual({555,555555,0},to_now("1987-08-10 00:59:15 GMT")),
        ?_assertEqual(ok, set_timezone("GMT")),
        ?_assertEqual({{1970, 1, 1}, {1, 0, 0}}, to_date("CET", "1970-01-01T00:00:00Z")),
        ?_assertEqual(ok, set_timezone("UTC")),
        ?_assertEqual(1521945120, to_unixtime("2018-3-25T2:32:00")),
        ?_assertEqual(true, between("-1 seconds", os:timestamp(), "+1 seconds")),
        ?_assertEqual(true, between("60 hours ago", unixtime(), "in 15 days")),
        ?_assertEqual(false, between("+1 seconds", qdate:to_string("n/j/Y g:ia"), "+2 seconds")),
        ?_assertEqual(false, between("5 seconds ago","1 second ago"))
    ]}.


simple_test(_) ->
    {inorder,[
        ?_assertEqual(ok,clear_timezone()),
        ?_assertEqual(0,to_unixtime({0,0,0})),
        ?_assertEqual({0,0,0},to_now(0)),
        ?_assertEqual(0,to_unixtime("1970-01-01 12:00am GMT")),
        ?_assertEqual(21600,to_unixtime("1970-01-01 12:00am CST")),
        ?_assertEqual(0,to_unixtime({{1970,1,1},{0,0,0}})),
        ?_assertEqual({{1970,1,1},{0,0,0}},to_date(0)),
        ?_assertEqual({{2013,3,7},{0,0,0}},to_date(to_unixtime("2013-03-07 12am"))),
        ?_assertEqual("2013-12-21 12:24pm",to_string("Y-m-d g:ia",{{2013,12,21},{12,24,21}})),
        ?_assertEqual("2012-12-01 1:00pm", to_string("Y-m-d g:ia","EST","2012-12-01 12:00pm CST")),
        ?_assertEqual(<<"2012-12-01 1:00pm">>, to_string(<<"Y-m-d g:ia">>,"EST","2012-12-01 12:00pm CST")),
        ?_assertEqual(<<"2012-12-01 1:00pm">>, to_string(<<"Y-m-d g:ia">>,"EST",<<"2012-12-01 12:00pm CST">>)),
        ?_assertEqual("2012-12-01 1:00pm", to_string("Y-m-d g:ia","EST",<<"2012-12-01 12:00pm CST">>)),
        ?_assertEqual("2012-12-01 1:00pm", to_string("Y-m-d g:ia",<<"EST">>,<<"2012-12-01 12:00pm CST">>)),
        ?_assertEqual(to_unixtime("2012-01-01 12:00pm CST"), to_unixtime("2012-01-01 10:00am PST")),
        ?_assertEqual({{2012,12,31},{18,15,15}},to_date("Dec 31, 2012 6:15:15pm")),
        ?_assertEqual({{2013,1,1},{0,15,15}},to_date("GMT", "December 31, 2012 6:15:15pm CST"))
    ]}.


compare_test(_) ->
    {inorder,[
        ?_assertEqual(true, compare({{2013,9,10},{0,0,0}},'=',"Sep 10th, 2013 12:00am")),
        ?_assertEqual(true, compare("9/10/2013 1am EDT",'==',"Sep 10th, 2013 12:00:00am CDT")),
        ?_assertEqual(true, compare({{2013,9,10},{0,0,0}},'=<',"Sep 10th, 2013 12:00am")),
        ?_assertEqual(false, compare({{2013,9,10},{0,0,1}},'=',"Sep 10th, 2013 12:00am")),
        ?_assertEqual(true, compare({{2013,9,10},{0,0,1}},'=/=',"Sep 10th, 2013 12:00am")),
        ?_assertEqual(true, compare({{2013,9,10},{0,0,1}},'>',"Sep 10th, 2013 12:00am")),
        ?_assertEqual(false, compare({{2013,9,10},{0,0,1}},'<',"Sep 10th, 2013 12:00am")),
        ?_assertEqual(true, compare({{2013,9,10},{0,0,1}},'<',"Sep 10th, 2013 12:00:02am")),
        ?_assertEqual(true, compare({{2013,9,10},{0,0,1}},'<',"Sep 10th, 2013 12:02am")),
        ?_assertEqual(true, compare({{2013,9,10},{0,0,1}},'<',"Sep 10th, 2013 1am")),
        ?_assertEqual(true, compare({{2013,9,9},{23,59,59}},'<',"Sep 10th, 2013 12am")),
        ?_assertEqual(false, compare({{2013,9,9},{23,59,59}},'>',"Sep 10th, 2013 12am")),
        ?_assertEqual(true, compare("11am EST",'==',"10am CST"))
    ]}.

parser_format_test(_) ->
    {inorder,[
        ?_assertEqual({{2008,2,8},{0,0,0}},to_date("20080208")),
        ?_assertThrow({ec_date,{bad_date,_}},to_date("20111232")),  %% invalid_date with custom format
        ?_assertEqual("2/8/2008",to_string(shortdate,{{2008,2,8},{0,0,0}})),
        ?_assertEqual("2/8/2008",to_string(shortdate,"20080208")), %% both regged format and parser
        ?_assertEqual("2/8/2008 12:00am",to_string(longdate,"2008-02-08 12:00am")),
        ?_assertEqual("2/8/2008 12:00am",to_string(longdate,"20080208"))
    ]}.
    
arith_tests(_) ->
    {inorder,[
        ?_assertEqual({{2012,2,29},{23,59,59}}, to_date(add_seconds(-1, {{2012,3,1},{0,0,0}}))),
        ?_assertEqual({{2013,2,28},{23,59,59}}, to_date(add_seconds(-1, {{2013,3,1},{0,0,0}}))),
        ?_assertEqual({{2015,1,1},{0,0,0}}, to_date(add_years(1, {{2014,1,1},{0,0,0}}))),
        ?_assertEqual({{2015,1,1},{0,0,0}}, to_date(add_seconds(1, {{2014,12,31},{23,59,59}}))),
        ?_assertEqual({{2015,1,1},{0,0,59}}, to_date(add_minutes(1, {{2014,12,31},{23,59,59}}))),
        ?_assertEqual({{2015,1,1},{0,59,59}}, to_date(add_hours(1, {{2014,12,31},{23,59,59}}))),
        ?_assertEqual({{2015,1,1},{23,59,59}}, to_date(add_days(1, {{2014,12,31},{23,59,59}}))),
        ?_assertEqual({{2015,1,7},{23,59,59}}, to_date(add_weeks(1, {{2014,12,31},{23,59,59}}))),
        ?_assertEqual({{2015,1,31},{23,59,59}}, to_date(add_months(1, {{2014,12,31},{23,59,59}}))),
        ?_assertEqual({{2015,2,28},{0,0,0}}, to_date(add_months(2, {{2014,12,31},{0,0,0}}))),
        ?_assertEqual({{2016,2,28},{0,0,0}}, to_date(add_years(1, {{2015,2,28},{0,0,0}}))),

        ?_assertEqual({{2017,2,1},{0,0,0}}, to_date(add_months(-11, {{2018,1,1},{0,0,0}}))),
        ?_assertEqual({{2017,1,1},{0,0,0}}, to_date(add_months(-12, {{2018,1,1},{0,0,0}}))),
        ?_assertEqual({{2016,12,1},{0,0,0}}, to_date(add_months(-13, {{2018,1,1},{0,0,0}}))),

        ?_assertEqual({{2018,12,1},{0,0,0}}, to_date(add_months(11, {{2018,1,1},{0,0,0}}))),
        ?_assertEqual({{2019,1,1},{0,0,0}}, to_date(add_months(12, {{2018,1,1},{0,0,0}}))),
        ?_assertEqual({{2019,2,1},{0,0,0}}, to_date(add_months(13, {{2018,1,1},{0,0,0}}))),

        ?_assertEqual({{2018,1,1},{0,0,0}}, to_date(add_months(-11, {{2018,12,1},{0,0,0}}))),
        ?_assertEqual({{2017,12,1},{0,0,0}}, to_date(add_months(-12, {{2018,12,1},{0,0,0}}))),
        ?_assertEqual({{2017,11,1},{0,0,0}}, to_date(add_months(-13, {{2018,12,1},{0,0,0}}))),

        ?_assertEqual({{2019,11,1},{0,0,0}}, to_date(add_months(11, {{2018,12,1},{0,0,0}}))),
        ?_assertEqual({{2019,12,1},{0,0,0}}, to_date(add_months(12, {{2018,12,1},{0,0,0}}))),
        ?_assertEqual({{2020,1,1},{0,0,0}}, to_date(add_months(13, {{2018,12,1},{0,0,0}}))),


        ?_assertEqual({{2014,2,28},{0,0,0}}, to_date(add_months(-24, {{2016,2,29},{0,0,0}}))),
        ?_assertEqual({{2018,12,15},{0,0,0}}, to_date(add_months(24, {{2016,12,15},{0,0,0}}))),
        ?_assertEqual({{2012,2,29},{0,0,0}}, to_date(add_months(-48, {{2016,2,29},{0,0,0}}))),
        ?_assertEqual({{2016,2,29},{0,0,0}}, to_date(add_months(-1, {{2016,3,31},{0,0,0}}))),
        ?_assertEqual({{2017,2,28},{0,0,0}}, to_date(add_years(1, {{2016,2,29},{0,0,0}}))),
        ?_assertEqual({{2015,3,1},{0,0,0}}, to_date(add_days(1, {{2015,2,28},{0,0,0}}))),
        ?_assertEqual({{2015,3,3},{0,0,0}}, to_date(add_days(3, {{2015,2,28},{0,0,0}}))),
        ?_assertEqual({{2017,1,2},{0,0,0}}, beginning_week({{2017,1,2},{0,0,0}})),
        ?_assertEqual({{2017,1,2},{0,0,0}}, beginning_week({{2017,1,3},{0,0,0}})),
        ?_assertEqual({{2017,1,3},{0,0,0}}, beginning_week(2, {{2017,1,4},{0,0,0}})),
        ?_assertEqual({{2016,12,29},{0,0,0}}, beginning_week(4, {{2017,1,4},{0,0,0}})),
        ?_assertEqual({{2016,12,31},{0,0,0}}, beginning_week(6, {{2017,1,6},{0,0,0}})),
        ?_assertEqual({{2017,1,1},{0,0,0}}, beginning_week(7, {{2017,1,6},{0,0,0}})),

        ?_assertEqual(0, age("1981-01-15", "1981-12-31")),
        ?_assertEqual(39, age("1981-01-15", "2020-01-15")),
        ?_assertEqual(39, age("1981-01-15", "2020-01-15 12am")),
        ?_assertEqual(38, age("1981-01-15", "2020-01-14")),
        ?_assertEqual(38, age("1981-01-15", "2020-01-14 11:59pm")),

        %% checking pre-unix-epoch
        ?_assertEqual(100, age("1901-01-01","2001-01-01")),
        ?_assertEqual(20, age("1900-01-01", "1920-01-01")),
        
        ?_assertEqual(0, age_days("2020-11-20 12am","2020-11-20 11:59:59pm")),
        ?_assertEqual(1, age_days("2020-11-19 11:59:59pm","2020-11-20 11:59:59pm")),
        ?_assertEqual(7, age_days("2020-11-20","2020-11-27")),
        ?_assertEqual(7, age_days("2020-11-27","2020-12-04"))

    ]}.

preserve_ms_true_tests(_) ->
    set_opt(preserve_ms, true),
    {inorder, [
      ?_assertEqual({{2021,5,8},{23,0,16,472000}}, qdate:to_date(<<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>)),
      ?_assertEqual(<<"2021-05-08T23:00:16.472000+00:00">>, qdate:to_string(<<"Y-m-d\\TH:i:s.fP">>, <<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>)),
      ?_assertEqual(<<"2021-05-08T23:00:16+00:00">>, qdate:to_string(<<"Y-m-d\\TH:i:sP">>, <<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>)),
      ?_assertEqual(<<"2021-05-08T23:00:16.472000+00:00">>, qdate:to_string(<<"c">>, <<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>))
    ]}.

preserve_ms_false_tests(_) ->
    set_opt(preserve_ms, false),
    {inorder, [
        ?_assertEqual({{2021,5,8},{23,0,16}}, qdate:to_date(<<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>)),
        ?_assertEqual(<<"2021-05-08T23:00:16.0+00:00">>, qdate:to_string(<<"Y-m-d\\TH:i:s.fP">>, <<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>)),
        ?_assertEqual(<<"2021-05-08T23:00:16+00:00">>, qdate:to_string(<<"Y-m-d\\TH:i:sP">>, <<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>)),
        ?_assertEqual(<<"2021-05-08T23:00:16+00:00">>, qdate:to_string(<<"c">>, <<"UTC">>,<<"2021-5-09 0:0:16.472+01:00">>))
    ]}.

start_test() ->
    qdate:start(),
    set_timezone(?SELF_TZ),
    set_timezone(?SITE_KEY,?SITE_TZ),
    set_timezone(?USER_KEY,?USER_TZ),
    register_parser(compressed,fun compressed_parser/1),
    register_parser(microsoft_date,fun microsoft_parser/1),
    register_parser(parse_relative, fun parse_relative/1),
    register_format(shortdate,"n/j/Y"),
    register_format(longdate,"n/j/Y g:ia").

compressed_parser(List) when length(List)==8 ->
    try re:run(List,"^(\\d{4})(\\d{2})(\\d{2})$",[{capture,all_but_first,list}]) of
        nomatch -> undefined;
        {match, [Y,M,D]} -> 
            Date = {list_to_integer(Y),list_to_integer(M),list_to_integer(D)},
            case calendar:valid_date(Date) of
                true ->
                    {Date,{0,0,0}};
                false -> undefined
            end
    catch
        _:_ -> undefined
    end;
compressed_parser(_) -> 
    undefined.

microsoft_parser(FloatDate) when is_float(FloatDate) ->
    try
        DaysSince1900 = flooring(FloatDate),
        Days0to1900 = calendar:date_to_gregorian_days(1900,1,1),
        GregorianDays = Days0to1900 + DaysSince1900,
        Date = calendar:gregorian_days_to_date(GregorianDays),
        Seconds = round(86400 * (FloatDate - DaysSince1900)),
        Time = calendar:seconds_to_time(Seconds),
        {Date,Time}
    catch
        _:_ -> undefined
    end;
microsoft_parser(_) ->
    undefined.

    

stop_test(_) ->
    ok.
