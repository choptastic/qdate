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

All while doing so by allowing you to either set a timezone by some arbitrary
key or by using the current process's Pid is the key.

Further, while `ec_date` doesn't support PHP's timezone characters (e, I, O, P,
T, Z, r, and c), `qdate` will handle them for us.

## Exported Functions:

### Conversion Functions

  + `to_string(FormatString, Date)` - "FormatString" is a string that follows the
    `date` function formatting rules. `Date` is any date, in almost any common
	format
  + `to_string(FormatString)` - Formats the current time
  + `to_date(Date)` - converts any date/time format to Erlang date format.
  + `to_now(Date)` - converts any date/time format to Erlang now format.
  + `to_unixtime(Date)` - converts any date/time format to a unixtime integer

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

## Example:

Calling `qdate:to_string` will allow single-line re-encoding of a date.

Say we wanted to convert the date and time from "12/31/2013 8:15pm" to
something like "2013-12-31 (16:15:00)":

Using just `ec_date`, you would do it like this:
```erlang
	OldDate = "12/31/2013 8:15pm",

   Date = ec_date:parse(OldDate),
   NewString = ec_date:format("Y-m-d (H:i:s)",Date).
```

With the new method, you could do it simply and clearly with one line:

```erlang
	NewString = qdate:to_string("Y-m-d (H:i:s)",OldDate).
```

The nice thing about it this though, is that OldDate can be *any* date format,
and it will figure it out. It can be any of the following:
  + Erlang Date Format: `{{Y,M,D},{H,M,S}}`
  + Erlang Now Format: `{MegaSecs, Secs, MicroSecs}`
  + Date String: `"2013-12-31 08:15pm"`
  + Integer Unix Timestamp: 1388448000
