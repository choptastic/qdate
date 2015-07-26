# qdate - Erlang Date and Timezone Library

[![Build Status](https://travis-ci.org/choptastic/qdate.png?branch=master)](https://travis-ci.org/choptastic/qdate)

## Purpose

Erlang Date and Time management is rather primitive, but improving.

[dh_date](https://github.com/daleharvey/dh_date), of which `ec_date` in 
[erlware_commons](https://github.com/erlware/erlware_commons) is a fork, is a
huge step towards formatting and parsing dates in a way that compares nicely
with PHP's [date](http://php.net/manual/en/function.date.php) and 
[strtotime](http://php.net/manual/en/function.strtotime.php) functions.

Unfortunately, `ec_date` doesn't deal with timezones, but conveniently, 
the project [erlang_localtime](https://github.com/dmitryme/erlang_localtime)
does.

It is the express purpose of this `qdate` package to bring together the
benefits of `ec_date` and `erlang_localtime`, as well as extending the
capabilities of both to provide for other needed tools found in a single
module.

`qdate` provides date and time formatting and parsing from and into:
 + Formatting Strings
 + Erlang Date Format
 + Erlang Now Format
 + Unixtime integers
 + Timezones

And all this while dealing with timezone parsing, formatting, conversion
and overall management.

#### Acceptable Date Formats

  + Erlang Date Format: `{{Y,M,D},{H,M,S}}`
  + Erlang Now Format: `{MegaSecs, Secs, MicroSecs}`
  + Date String: `"2013-12-31 08:15pm"` (including custom formats as defined
    with `qdate:register_parser/2` - see below)
  + Integer Unix Timestamp: 1388448000
  + A Two-tuple, where the first element is one of the above, and the second
    is a timezone.  (i.e. `{{{2008,12,21},{23,59,45}}, "EST"}` or
    `{"2008-12-21 11:59:45pm", "EST"}`). **Note:** While, you can specify a
    timezone along with unix timestamps or the Erlang now format, it won't do
    anything, as both of those timestamps are absolute, and imply GMT.


All while doing so by allowing you to either set a timezone by some arbitrary
key or by using the current process's Pid is the key.

Further, while `ec_date` doesn't support PHP's timezone characters (e, I, O, P,
T, Z, r, and c), `qdate` will handle them for us.

## Exported Functions:

### Conversion Functions

  + `to_string(FormatString, ToTimezone, Date)` - "FormatString" is a string
    that follows PHP's `date` function formatting rules. The date will be
    converted to the specified `ToTimezone`.
  + `to_string(FormatString, Date)` - same as `to_string/3`, but the `Timezone`
    is intelligently determined (see below)
  + `to_string(FormatString)` - same as `to_string/2`. but uses the current
    time as `Date`
  + `to_date(ToTimezone, Date)` - converts any date/time format to Erlang date
    format. Will first convert the date to the timezone `ToTimezone`.
  + `to_date(Date)` - same as `to_date/2`, but the timezone is determined (see below).
  + `to_now(Date)` - converts any date/time format to Erlang now format.
  + `to_unixtime(Date)` - converts any date/time format to a unixtime integer

A **ToTimezone** value of the atom `auto` will automatically determine the
timezone. For example, `to_date(Date, auto)` is exactly the same as
`to_date(Date)`

**A Note About Argument Order**: In all cases, `ToTimezone` is optional and if
omitted, will be determined as described below in "Understanding Timezone
Determining and Conversion". If `ToTimezone` is specified, it will always be
immediately left of the `Disambiguate` argument (if it's specified), which is
always immediately left of `Date` argument. `Date` will always be the last
argument to any of the conversion and formatting functions.

#### Understanding Timezone Determining and Conversions

There is a lot of timezone inferring going on here.

If a `Date` string contains timezone information (i.e.
`"2008-12-21 6:00pm PST"`), then `qdate` will parse that properly, determine
the specified `PST` timezone, and do conversions based on that timezone.
Further, you can specify a timezone manually, by specifying it as as a
two-tuple for `Date` (see "Acceptable Date formats" above).

If no timezone is specified or determinable in a `Date` variable, then `qdate`
will infer the timezone in the following order.

  + If specified by `qdate:set_timezone(Timezone)` for that process. Note, as 
    specified below (in the "Timezone Functions" section), `set_timezone/1` is
    a shortcut to `set_timezone(self(), Timezone)`, meaning that
    `set_timezone/1` only applies to that *specific* process. If none is
    specified.
  + If no timezone is specified for the process, `qdate` looks at the `qdate`
    application variable `default_timezone`.
  + If no timezone is specified by either of the above, `qdate` assumes "GMT"
    for all dates.
  + A timezone value of `auto` will act as if no timezone is specified.

#### Disambiguating Ambiguous Timezone Conversions

Sometimes, when youre converting a datetime from one timezone to another, there
are potentially two different results if the conversion happens to land on in a
timezone that's in the middle of a Daylight Saving conversion.  For example,
converting "11-Nov-2013 1:00:am" in "America/New York" to "GMT" could be both
"5am" and "6am" in GMT, since "1am EST". This is a side effect of the
"intelligence" of `qdate` - `qdate` would notice that 1am in New York is EST,
and should be converted to "1am EST", and then do the conversion from "1am EST"
to "GMT".  This can lead to confusion.

Further, since `qdate` attempts to be "smart" about mistakenly entered
timezones (ie, if you entered "2013-01-01 EDT", `qdate` knows that "EDT"
(Eastern Daylight Time) doesn't apply to January first, so it *assumes* you
meant "EST".

**THE SOLUTION** to this tangled mess that we call Daylight Saving Time is to
provide an option to disambiguate if you so desire. By default disambiguation
is disabled, and `qdate` will just guess as to it's best choice. But if you so
desire, you can make sure qdate does *both* conversions, and returns both.

You can do this by passing a `Disambiguation` argument to `to_string`,
`to_date`, `to_unixtime`, and `to_now`. `Disambiguation` can be an atom of the
values:

  + `prefer_standard` *(Default Behavior)*: If an ambiguous result occurs,
    qdate will return the date in standard time rather than daylight time.
  + `prefer_daylight`: If an ambiguous result occurs, qdate will return the
    preferred daylight time.
  + `both`: If an ambiguous result occurs, `qdate` will return the tuple:
    `{ambiguous, DateStandard, DateDaylight}`, where `DateStandard` is the date
	in Standard Time, and `DateDaylight` is the date in Daylight Saving Time.

So the expanded conversions functions are:

  + `to_date(ToTimezone, Disambiguate, Date)`
  + `to_string(FormatString, ToTimezone, Disambiguate, Date)`
  + `to_unixtime(Disambiguate, Date)`
  + `to_now(Disambiguate, Date)`

Examples:

```erlang
1> qdate:set_timezone("GMT").
ok

%% Here, converting GMT 2013-11-03 6AM to America/New York yields an ambiguous
%% result
2> qdate:to_date("America/New York", both, {{2013,11,3},{6,0,0}}).
{ambiguous,{{2013,11,3},{1,0,0}},{{2013,11,3},{2,0,0}}}

%% Let's just use daylight time
3> qdate:to_date("America/New York", prefer_daylight, {{2013,11,3},{6,0,0}}).
{{2013,11,3},{2,0,0}}

%% Let's just use standard time (the default behavior)
4> qdate:to_date("America/New York", prefer_standard, {{2013,11,3},{6,0,0}}).
{{2013,11,3},{1,0,0}}

5> qdate:set_timezone("America/New York").
ok

%% Switching from 1AM Eastern Time to GMT yields a potentially ambiguous result
6> qdate:to_date("GMT", both, {{2013,11,3},{1,0,0}}).
{ambiguous,{{2013,11,3},{6,0,0}},{{2013,11,3},{5,0,0}}}

%% Use daylight time for conversion
7> qdate:to_date("GMT", prefer_daylight, {{2013,11,3},{1,0,0}}).
{{2013,11,3},{5,0,0}}

%% Here we demonstrated that even if we ask for "both", if there is no
%% ambiguity, the plain date is returned
8> qdate:to_date("GMT", both, {{2013,11,3},{5,0,0}}).
{{2013,11,3},{10,0,0}}
```

#### Conversion Functions provided for API compatibility with `ec_date`

  + `parse/1` - Same as `to_date(Date)`
  + `nparse/1` - Same as `to_now(Date)`
  + `format/1` - Same as `to_string/1`
  + `format/2` - Same as `to_string/2`

### Date and Time Comparison

`qdate` provides a few convenience functions for performing date comparisons.

  + `compare(A, B)` - Like C's `strcmp`, returns:
    + `0`: `A` and `B` are exactly the same.
    + `-1`: `A` is less than (before) `B`.
    + `1`: `A` is greater than (after) `B`.
  + `compare(A, Operator, B)` - Operator is an infix comparison operator, and
    the function will return true if:
    + `'='`, or `'=='` - `A` is the same time as `B`
    + `'/='`, or `'=/='` or `'!='` - `A` is not the same time as `B`
    + `'<'` - `A` is before `B`
    + `'>'` - `A` is after `B`
    + `'=<'` or `'<='` - `A` is before or equal to `B`
    + `'>='` or `'=>'` - `A` is after or equal to `B`

**Note 1:** `Operator` must be an atom.

**Note 2:** These functions will properly compare times with different timezones
(for example: `compare("12am CST",'==',"1am EST")` will properly return true)

### Timezone Functions

  + `set_timezone(Key, TZ)` - Set the timezone to TZ for the key `Key`
  + `set_timezone(TZ)` - Sets the timezone, and uses the Pid from `self()` as
    the `Key`. Also links the process for removal from the record when the Pid
    dies.
  + `get_timezone(Key)` - Gets the timezone assigned to `Key`
  + `get_timezone()` - Gets the timezone using `self()` as the `Key`
  + `clear_timezone(Key)` - Removes the timezone record associated with `Key`.
  + `clear_timezone()` - Removes the timezone record using `self()` as `Key`.
    This function is not necessary for cleanup, most of the time, since if
    `Key` is a Pid, the `qdate` server will automatically clean up when the
    Pid dies.

**Note:** If no timezone is set, then anything relying on the timezone will
default to GMT.

### Registering Custom Parsers and Formatters

You can register custom parsers and formatters with the `qdate` server. This
allows you to specify application-wide aliases for certain common formatting
strings in your application, or to register custom parsing engines which will
be attempted before engaging the `ec_date` parser.

### Registering and Deregistering Parsers
  + `register_parser(Key, ParseFun)` - Registers a parsing function with the
    `qdate` server. `ParseFun` is expected to have the arity of 1, and is
    expected to return a DateTime format (`{{Year,Month,Day},{Hour,Min,Sec}}`)
    or, if your ParseFun is capable of parsing out a Timezone, the return
    the tuple `{DateTime, Timezone}`. Keep in mind, if your string already ends
    with a Timezone, the parser will almost certainly extract the timezone
    before it gets to your custom `ParseFun`. If your custom parser is not
    able to parse the string, then it should return `undefined`.
  + `deregister_parser(Key)` - If you previously registered a parser with the
    `qdate` server, you can deregister it by its `Key`.
  + `get_parsers()` - Get the list of all registered parsers and their keys.

### Registering and Deregistering Formatters
  + `register_format(Key, FormatString)` - Register a formatting string with
    the `qdate` server, which can then be used in place of the typical
    formatting string.
  + `deregister_format(Key)` - Deregister the formatting string from the
    `qdate` server.
  + `get_formats()` - Get the list of all registered formats and their keys.

### About backwards compatibility with `ec_date` and deterministic parsing

`ec_date` and `dh_date` both have a quirk that bothers me with respect to the
parsing of dates that causes some date parsing to be *non-deterministic*. That
is, if parsing an incomplete date or time (ie, a text string that is missing a
time or a date), `ec_date` will automatically insert the current values of
those as read by the system clock.

For example, if the following lines are run a few seconds apart:

```erlang
1> ec_date:parse("2012-02-04").
{{2012,2,4},{0,1,10}}
2> ec_date:parse("2012-02-04").
{{2012,2,4},{0,1,12}}
3> ec_date:parse("2012-02-04").
{{2012,2,4},{0,1,13}}
```

As you can see, even though the inputs are the same each time, the resulting
parsed dates have the current time inferred. The same behavior can be observed
if parsing a time without a date:

```erlang
4> ec_date:parse("7pm").
{{2013,4,30},{19,0,0}}
```

As you can see, even though the time did not specify a date, the resulting
parsed datetime has the date inferred from the current date. Admittedly,
inferring the date bothers me less than inferring the time, but in the name of
consistency, there should be options for enabling or disabling both.

#### The Solution For Non-deterministic parsing

To solve this issue for users that are bothered by this, while preserving
backwards compatibility for folks who prefer this, we're going to introduce a
`qdate` application environment variable called `deterministic_parsing`.

The value of `deterministic_parsing` can be a tuple of the following format:

`{DatePref, TimePref}`

Where `DatePref` and `TimePref` are either of the following atoms:

  + `now` - Automatically fill in the missing date or time components with the
    current time (the is the behavior described above)
  + `zero` - Fill in the missing date or time components with zeroed out
    values. This means that if a date is missing, it'll be set to the unix
    epoch (`{1970,1,1}`) and if a time is missing, it'll be set to midnight:
    `{0,0,0}`.

So, the acceptable combinations can be the following:

  + `{zero, zero}` - Any missing components will be replaced with zero-values.
    **(This is the qdate default behavior)**
  + `{now, zero}` - If a date is missing, insert the current date, but if a
    time is missing, set it to midnight.
  + `{zero, now}` - If a date is missing, set it to the unix epoch, and if a
    time is missing, set it to the current time of day.
  + `{now, now}` - If either date or time are missing, set it to the current
    date or current time.

**Note:** If this application value is not set, the default behavior for
`qdate` is to avoid non-determinism and use `{zero, zero}`.

To set this value, you can either set the value manually in code with:

```erlang
application:set_env(qdate, deterministic_parsing, {now, zero}).
```

or (and this is the preferred method) use a config file and load it with

`erl -config path/to/file.config`

Sample config file specifying this application variable:

```erlang
[{qdate, [
    {deterministic_parsing, {now, zero}}
]}].
```

## Demonstration

### Basic Conversion and Formatting
```erlang
%% Let's start by making a standard Erlang DateTime tuple
1> Date = {{2013,12,21},{12,24,21}}.
{{2013,12,21},{12,24,21}}

%% Let's do a simple formatting of the date
2> DateString = qdate:to_string("Y-m-d h:ia", Date).
"2013-12-21 12:24pm"

%% We can also specify the format string as a binary
3> DateBinary = qdate:to_string(<<"Y-m-d h:ia">>,Date).
<<"2013-12-21 12:24pm">>

%% And we can parse the original string to get back a DateTime object
4> qdate:to_date(DateString).
{{2013,12,21},{12,24,0}}


%% We can do the same with a binary
5> qdate:to_date(DateBinary).
{{2013,12,21},{12,24,0}}

%% We can also parse that date and get a Unix timestamp
6> DateUnix = qdate:to_unixtime(DateString).
1387628640

%% And we can take that Unix timestamp and format it to a string
7> qdate:to_string("n/j/Y g:ia", DateUnix).
"12/21/2013 12:24pm"

%% We can take a date string and get an Erlang now() tuple
8> DateNow = qdate:to_now(DateString).
{1387,628640,0}

%% And we can convert it back

9> DateString2 = qdate:to_string("n/j/Y g:ia", DateNow).
"12/21/2013 12:24pm"
```

**Note:** That by this point, we've used, as the `Date` parameter, all natively
supported date formats: Erlang `datetime()`, Erlang `now()`, Unix timestamp,
and formatted text strings either as a list or as a binary.

For the most part, this will be the bread and butter usage of `qdate`.  Easily
converting from one format to another without having to worry about what format
your data is currently in. `qdate` will figure it out for you.

*But now, we're going to start getting interesting!*

### Registering Custom Parsers

```erlang
%% Let's format our date into something shorter. This may, for example, be a
%% date format you may deal with when receiving a data-set from a client.
10> CompactDate = qdate:to_string("Ymd", DateNow).
"20131221"

%% Let's try to parse it
11> qdate:to_date(CompactDate).
** exception throw: {ec_date,{bad_date,"20131221"}}
     in function  ec_date:do_parse/3 (src/ec_date.erl, line 92)
     in call from qdate:to_date/2 (src/qdate.erl, line 169)

%% Well obviously, this isn't a standard format by any means, so it crashes.
%% You can parse it yourself before passing it to `qdate` or if you deal with
%% this format frequently enough, you can register it as a custom parser and
%% qdate will intelligently parse it if it can.

%% So let's make a simple parser for it that uses regular expressions:
12> ParseCompressedDate =
12>  fun(RawDate) when length(RawDate)==8 ->
12>       try re:run(RawDate,"^(\\d{4})(\\d{2})(\\d{2})$",[{capture,all_but_first,list}]) of
12>         nomatch -> undefined;
12>         {match, [Y,M,D]} ->
12>           ParsedDate = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
12>           case calendar:valid_date(ParsedDate) of
12>              true -> {ParsedDate, {0,0,0}};
12>              false -> undefined
12>           end
12>       catch _:_ -> undefined
12>       end;
12>     (_) -> undefined
12>  end.
#Fun<erl_eval.6.82930912>

%% And now we'll register the parser with the `qdate` server, giving it a "Key"
%% of the atom 'compressed_date'
13> qdate:register_parser(compressed_date,ParseCompressedDate).
compressed_date

%% Now, let's try parsing that again
14> qdate:to_date(CompactDate).
{{2013,12,21},{0,0,0}}

%% Huzzah! It worked. From here on out, `qdate`, will properly parse that kind
%% of data if that format is passed, otherwise, it will merely skip over that
%% parser and engage the standard parser in `ec_date`
```

**Note:** Currently, `qdate` expects custom parsers to not crash. If a custom
parser crashes, an exception will be thrown. This is done in order to help you
debug your parsers. If a parser receives an unexpected input and crashes, the
exception will be generated and you will be able to track down what input caused
the crash.

**Another Note:** Custom parsers are expected to return either:
  + A `datetime()` tuple. (ie {{2012,12,21},{14,45,23}}).
  + The atom `undefined` if this parser is not a match for the supplied value


### Registering Custom Formats

```erlang
%% Let's format a date to a rather long string
15> qdate:to_string("l, F jS, Y g:i A T",DateString).
"Saturday, December 12st, 2013 12:24 PM GMT"

%% Boy, that sure was a long string, I hope you can remember all those
%% characters in that order!

%% But, you don't have to: if that's a common format you use in your
%% application, you can register your format with the `qdate` server, and then
%% easiy refer to that format by its key.

%% So let's take that format and register it
16> qdate:register_format(longdate, "l, F jS, Y g:i A T").
ok

%% Now, let's try to format our string 
17> LongDateString = qdate:to_string(longdate, DateString).
"Saturday, December 21st, 2013 12:24 PM GMT"

%% It was certainly easier to remember the atom 'longdate' than trying to
%% remember the seemingly random "l, F jS, Y g:i A T".
```

Ain't it nice, making things easier for you?

### Timezone Demonstrations

The observant reader would have noticed something else. We used **timezones**
in the last couple of calls. Indeed, not only can `qdate` deal with formatting
timezones, but it can also parse them, convert them, and set them for
simplified conversions.

Let's see how we do this

```erlang
%% Let's take that last long date string (that was in GMT) and move it to
%% Pacific time
18> LongDatePDT = qdate:to_string(longdate, "PDT", LongDateString).
"Saturday, December 21st, 2013 4:24 AM PST"

%% See something interesting there? Yeah, we told it it was PDT, but it output
%% PST.  That's because PST is not in daylight saving time in December, and 
%% `qdate` was able to intelligently infer that, and fix it for us.

%% Note, that when in doubt, `qdate` will *not* convert. For example, not all
%% places in Eastern Standard Time do daylight saving time, and as such, EST
%% will not necessarily convert to EDT.

%% However, if you provide the timezone as something like "America/New York",
%% it *will* figure that out, and do the correct conversion for you. 

%% Let's see how it handles unix times with strings that contain timezones.
%% If you recall, LongDateString = "Saturday, December 21st, 2013 12:24 PM GMT"
%% and LongDatePDT = "Saturday, December 21st, 2013 4:24 AM PST"
19> qdate:to_unixtime(LongDateString).
1387628640

%% Now let's try it with the Pacific Time one
20> qdate:to_unixtime(LongDatePDT).
1387628640

%% How exciting! `qdate` properly returned the same unix timestamp, since unix
%% timestamps are timezone neutral. That is because unix timestamps are the
%% number of seconds since midnight on 1970-01-01 GMT. As such, unix timestamps
%% should not change, just because you're in a different timezone.

%% Let's set the timezone for the current process to EST to test that previous
%% assertion
21> qdate:set_timezone("EST").
ok

%% Now let's try converting those dates to unixtimes again
22> qdate:to_unixtime(LongDateString).
1387628640
23> qdate:to_unixtime(LongDatePDT).
1387628640

%% Great! They didn't change, as we expected. The unix timestamps have remained
%% Timezone neutral.

%% Let's clear the current process's timezone (which basically means setting it
%% to the application variable `default_timezone`, or, in this case, just
%% resetting it to "GMT"
24> qdate:clear_timezone().
ok

%% Now, let's imagine you run a website. The main site may have its own
%% timezone, and the users each also have their own timezones.  So we'll
%% register timezones for each the main site, and each user. That way, if we
%% need to ensure that a date is presented in an appropriate timezone.


%% Let's register some timezones by "Timezone Keys".  
25> qdate:set_timezone(my_site, "America/Chicago").
ok
26> qdate:set_timezone({user,1},"Australia/Melbourne").
ok

%% So we'll get the date object of the previously set unix timestamp `DateUnix`
27> qdate:to_date(DateUnix).
{{2013,12,21},{12,24,0}}

%% And let's format it, also showing the timezone offset that was used
28> qdate:to_string("Y-m-d H:i P", DateUnix).
"2013-12-21 12:24 +00:00"

%% Since we cleared the timezone for the current process, it just used "GMT"

%% Let's get the date again, but this time, use to the Timezone key `my_site`
29> qdate:to_date(my_site, DateUnix).
{{2013,12,21},{6,24,0}}

%% And let's format it to show again the timezone offset
30> qdate:to_string("Y-m-d H:i P", my_site, DateUnix).
"2013-12-21 06:24 -06:00"

%% Finally, let's get the date using the User's timezone key
31> qdate:to_date({user,1}, DateUnix).
{{2013,12,21},{23,24,0}}

%% And again, formatted to show the timezone offset
32> UserDateWithHourOffset = qdate:to_string("Y-m-d H:i P", {user,1}, DateUnix).
"2013-12-21 23:24 +11:00"

%% And finally, let's just test some more parsing and converting. Here, despite
%% the fact that the timezone is presented as "+11:00", `qdate` is able to
%% do the proper conversion, and give us back the same unix timestamp that was
%% used.
33> qdate:to_unixtime(UserDateWithHourOffset).
1387628640
```

### One last bit of magic that may confuse you without an explanation

Magic is usually bad, you know what's worse? Timezones and Daylight Saving
Time. So we use a little magic to try and simplify them for us. Below is the
extent of the confusion with related to inferring timezones and formatting dates

```erlang
%% First, let's set the timezone to something arbitrary
34> qdate:set_timezone("EST").
ok

%% Let's convert this date to basically the same time format, just without the
%% timezone identifier.
35> qdate:to_string("Y-m-d H:i","2012-12-21 00:00 PST").
"2012-12-21 03:00"

%% WHAT?! We entered a date and time, and out came a different time?!
%% I CALL SHENANIGANS!

%% Let's add that timezone indicator back in with the conversion to see what
%% happened:

36> qdate:to_string("Y-m-d H:i T","2012-12-21 00:00 PST").
"2012-12-21 03:00 EST"

%% OOOOOOOHHH! I see!
%% Because we set our current timezone to EST, it took the original provided
%% date in PST, and converted it to EST (since EST is the timezone we've chosen
%% for the current process). So it's taking whatever date, and if it can
%% determine a timezone, it'll extract that timezone, and convert the time from
%% that timezone to our intended timezone.
```

## Date Arithmetic

(not fully tested yet, but will have full tests for 0.4.0)

The current implementation of qdate's date arithmetic returns Unixtimes.

There are 8 main functions for date arithmetic:

	+ `add_seconds(Seconds, Date)`
	+ `add_minutes(Minutes, Date)`
	+ `add_hours(Hours, Date)`
	+ `add_days(Days, Date)`
	+ `add_weeks(Weeks, Date)`
	+ `add_months(Months, Date)`
	+ `add_years(Years, Date)`
	+ `add_date(DateToAdd, Date)` - `DateToAdd` is a shortcut way of adding
	  numerous options. For example. `qdate:add_date({{1, 2, -3}, {-500, 20, 0}})`
	  will add 1 year, add 2 months, subtract 3 days, subtract 500 hours, add 20
	  minutes, and not make any changes to seconds.

For the date arithmetic functions, `Date`, like all `qdate` functions, can be any
format.

### Date Arithmetic from "now"

There are 7 other arithmetic functions that take a single argument, and these do arithmetic from "now." For example, `add_years(4)` is a shortcut for `add_years(4, os:timestamp())`.

   + `add_seconds(Seconds)`
   + `add_minutes(Minutes)`
   + `add_hours(Hours)`
   + `add_days(Days)`
   + `add_weeks(Weeks)`
   + `add_months(Months)`
   + `add_years(Years)`

## Thanks

A few shoutouts to [Dale Harvey](http://github.com/daleharvey) and the
[Erlware team](https://github.com/erlware) for `dh_date`/`ec_date`, and to
[Dmitry Melnikov](https://github.com/dmitryme) for the `erlang_localtime`
package. Without the hard work of all involved in those projects, `qdate` would
not exist.

### Thanks to Additional Contributors

+ [Mark Allen](https://github.com/mrallen1)
+ [Christopher Phillips](https://github.com/lostcolony)
+ [Nicholas Lundgaard](https://github.com/nlundgaard-al)
+ [Alejandro Ramallo](https://github.com/aramallo)
+ [Heinz Gies](https://github.com/Licenser)


## Changelog

See [CHANGELOG.markdown](https://github.com/choptastic/qdate/blob/master/CHANGELOG.markdown)

## TODO

+ Make `qdate` backend-agnostic (allow specifying either ec_date or dh_date as
  the backend)
+ Add `-spec` and `-type` info for dialyzer
+ Provide a sample qdate.config for users to see
+ Research the viability of [ezic](https://github.com/drfloob/ezic) for a
  timezone backend replacement for `erlang_localtime`.

## Conclusion

I hope you find `qdate` helpful in all your endeavors and it helps make your
wildest dreams come true!

If you have any bugs, feature requests, or whatnot, feel free to post a Github
issue, ping me on Twitter, or email me below.

I'm open to pull requests. Feel free to get your hands dirty!

Author: [Jesse Gumm](http://sigma-star.com/page/jesse)

Email: gumm@sigma-star.com

Twitter: [@jessegumm](http://twitter.com/jessegumm)

Released under the MIT License (see LICENSE file)
