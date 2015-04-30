## 0.4.0 (in-development)

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
