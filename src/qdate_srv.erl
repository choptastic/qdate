% vim: ts=4 sw=4 et
% Copyright (c) 2013-2015 Jesse Gumm
% See LICENSE for licensing information.
%
% NOTE: You'll probably notice that this isn't *actually* a server.  It *used*
% to be a server, but is now instead just where we interact with the qdate
% application environment.  Anyway, sorry for the confusion.

-module(qdate_srv).

-export([
    set_timezone/1,
    set_timezone/2,
    get_timezone/0,
    get_timezone/1,
    clear_timezone/0,
    clear_timezone/1,

    register_parser/1,
    register_parser/2,
    get_parsers/0,
    deregister_parsers/0,
    deregister_parser/1,

    register_format/2,
    get_format/1,
    deregister_format/1,
    get_formats/0
]).

%% Simple wrappers for unique keys
-define(BASETAG, qdate_var).
-define(KEY(Name), {?BASETAG, Name}).

-define(TZTAG, qdate_tz).
-define(TZKEY(Name), {?TZTAG, Name}).
-define(PARSERTAG, qdate_parser).
-define(PARSERKEY(Name), {?PARSERTAG, Name}).
-define(FORMATTAG, qdate_format).
-define(FORMATKEY(Name), {?FORMATTAG, Name}).

%% PUBLIC API FUNCTIONS

set_timezone(TZ) ->
    put_pd(?TZTAG, TZ).

set_timezone(Key, TZ) ->
    set_env(?TZKEY(Key), TZ).

get_timezone() ->
    get_pd(?TZTAG).

get_timezone(Key) ->
    get_env(?TZKEY(Key)).

clear_timezone() ->
    unset_pd(?TZTAG).

clear_timezone(Key) ->
    unset_env(?TZKEY(Key)).

register_parser(Parser) when is_function(Parser,1) ->
    register_parser(erlang:make_ref(),Parser).

register_parser(Key,Parser) when is_function(Parser,1) ->
    set_env(?PARSERKEY(Key), Parser).

deregister_parser(Key) ->
    unset_env(?PARSERKEY(Key)).

deregister_parsers() ->
    [deregister_parser(Key) || {Key, _} <- get_parsers()].

get_parsers() ->
    get_all_env(?PARSERTAG). 

register_format(Key, Format) ->
    set_env(?FORMATKEY(Key), Format).

get_format(Key) ->
    get_env(?FORMATKEY(Key)).

deregister_format(Key) ->
    unset_env(?FORMATKEY(Key)).
   
get_formats() ->
    get_all_env(?FORMATTAG).

%% PRIVATE TOOLS

%% App Vars

set_env(Key, Val) ->
    application:set_env(qdate, ?KEY(Key), Val).

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    %% Soon, this can just be replaced with application:get_env/3
    %% which was introduced in R16B.
    case application:get_env(qdate, ?KEY(Key)) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

unset_env(Key) ->
    application:unset_env(qdate, ?KEY(Key)).

get_all_env(FilterTag) ->
    All = application:get_all_env(qdate),
    %% Maybe this is a little nasty.
    [{Key, V} || {{?BASETAG, {Tag, Key}}, V} <- All, Tag==FilterTag].

%% ProcDic Vars

get_pd(Key) ->
    erlang:get(?KEY(Key)).

put_pd(Key, Val) ->
    erlang:put(?KEY(Key), Val),
    ok.

unset_pd(Key) ->
    put_pd(Key, undefined).
