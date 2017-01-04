## 0.5.0 (in development)

* Add `range_X` functions for getting a list of dates/times within a range
  (such as `range_day/3` to get a range of days between a start and end date.
* Add `beginning_X` functions to return the beginning of the provided precision
  (minute, hour, day, week, month, or year)
* Add `between/[2,3,5]` functions for computing whether a date/time is between
  two others.
* Update to rebar3 and add hex compatability. (@Licenser)
* Properly add dependent apps to .app.src (@Licenser)
* Add an optional "relative date/time parser".
* Fix: Ensure `get_timezone()` returns the default timezone (from config) if it
  hasn't been set by `get_timezone()`
* Remove R14 from travis testing.

## 0.4.2

* Add partial support for `ec_date`'s 4-tuple subsecond accuracy time format.
* Fix `erlware_commons` dependency to a rebar2-compatible version.

## 0.4.1

* Remove unnecessary `io:format` call.

## 0.4.0

* Remove dependency on a running server for tracking application state.
  Instead, parsers and formats are registered to the application environment
  vars (e.g. `application:get_env`), and timezones are registered to the
  application environment or the process dictionary. A side-effect of this
  change is that you can no longer query another process's timezone. 
* Add basic date arithmetic (e.g. `qdate:add_hours/[1-2]`, etc).
* Add `get_formats()` and `get_parsers()` to see list of registered formats and
  parsers.
* Fix bug related to relying on the application environment variable
  `default_timezone`

## 0.3.0

* Add Timezone/Daylight Saving Disambiguation
* Add the `auto` timezone shortcut
* Fix rebar.config to allow for compilation on Erlang 17

## 0.2.1

* Fix allowing timezone names to be binary

## 0.2.0

* Adding `qdate:compare/2,3` for easily comparing dates

## 0.1.0

* Initial Release
