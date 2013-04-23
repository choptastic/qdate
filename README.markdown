# qdate - A Wrapper for Erlang Date/Time Management

## Purpose

Erlang Date and Time management is rather primitive, but improving.

[dh_date](https://github.com/daleharvey/dh_date), of which `ec_date` in 
[erlware_commons](https://github.com/erlware/erlware_commons) is a huge step
towards formatting and parsing dates in a way that compares nicely with PHP's
[date](http://php.net/manual/en/function.date.php) and 
[strtotime](http://php.net/manual/en/function.strtotime.php) functions.

Unfortunately, `ec_date` doesn't deal with timezones, but conveniently, 
the project [erlang_localtime](https://github.com/dmitryme/erlang_localtime)
does.

It is the express purpose of this `qdate` package to bring together the
benefits of `ec_date` and `erlang_localtime`, as well as extending the
capabilities of both to provide for other needed tools found in a single
module.

`qdate` will provide, under the roof of a single module date and time formatting
and parsing from and into:
 + Formatting Strings
 + Erlang Date Format
 + Erlang Now Format
 + Unixtime integers

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
  + `to_date(Date, ToTimezone)` - converts any date/time format to Erlang date
    format. Will first convert the date to the timezone `ToTimezone`.
  + `to_date(Date)` - same as `to_date/2`, but the timezone is determined (see below).
  + `to_now(Date)` - converts any date/time format to Erlang now format.
  + `to_unixtime(Date)` - converts any date/time format to a unixtime integer

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

#### Conversion Functions provided for API compatibility with `ec_date`

  + `parse/1` - Same as `to_date(Date)`
  + `nparse/1` - Same as `to_now(Date)`
  + `format/1` - Same as `to_string/1`
  + `format/2` - Same as `to_string/2`

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

### Registering and Deregistering Formatters
  + `register_format(Key, FormatString)` - Register a formatting string with
    the `qdate` server, which can then be used in place of the typical
    formatting string.
  + `deregister_format(Key)` - Deregister the formatting string from the
    `qdate` server.

## Demonstration


