% vim: ts=4 sw=4 et
% Copyright (c) 2013-2021 Jesse Gumm
% See LICENSE for licensing information.

-module(qdate_srv).
-behaviour(gen_server).

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


%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% Simple wrappers for unique keys
-define(BASETAG, qdate_var).
-define(KEY(Name), {?BASETAG, Name}).

-define(TZTAG, qdate_tz).
-define(TZKEY(Name), {?TZTAG, Name}).
-define(PARSERTAG, qdate_parser).
-define(PARSERKEY(Name), {?PARSERTAG, Name}).
-define(FORMATTAG, qdate_format).
-define(FORMATKEY(Name), {?FORMATTAG, Name}).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    error_logger:info_msg("Creating qdate ETS Table: ~p",[?TABLE]),
    ?TABLE = ets:new(?TABLE, [public, {read_concurrency, true}, named_table]),
    {ok, #state{}}.

handle_call({set, Key, Val}, _From, State) ->
    ets:insert(?TABLE, {Key, Val}),
    {reply, ok, State};
handle_call({unset, Key}, _From, State) ->
    ets:delete(?TABLE, Key),
    {reply, ok, State};
handle_call(_, _From, State) ->
    {reply, invalid_request, State}.

handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.



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
    gen_server:call(?SERVER, {set, ?KEY(Key), Val}).

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case ets:lookup(?TABLE, ?KEY(Key)) of
        [{__Key, Val}] -> Val;
        [] -> Default
    end.

unset_env(Key) ->
    gen_server:call(?SERVER, {unset, ?KEY(Key)}).

get_all_env(FilterTag) ->
    try ets:tab2list(?TABLE) of
        All ->
            [{Key, V} || {{?BASETAG, {Tag, Key}}, V} <- All, Tag==FilterTag]
    catch
        error:badarg:S ->
            Msg = maybe_start_msg(),
            logger:error(Msg),
          
            error({qdate_get_all_env_failed, #{
                original_error => {error, badarg, S}
            }})
    end.

maybe_start_msg() ->
    "Attempting to read qdate environment failed (qdate_srv:get_all_env/1).\n"
    "qdate may not have been started properly.\n"
    "Please ensure qdate is started properly by either:\n"
    "* Putting qdate in the 'applications' key in your .app.src or .app file, or\n"
    "* Starting it manually with application:ensure_all_started(qdate) or qdate:start().".


%% ProcDic Vars

get_pd(Key) ->
    erlang:get(?KEY(Key)).

put_pd(Key, Val) ->
    erlang:put(?KEY(Key), Val),
    ok.

unset_pd(Key) ->
    put_pd(Key, undefined).






