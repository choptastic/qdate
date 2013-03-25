-module(qdate_srv).
-behaviour(gen_server).

-define(SRV, ?MODULE).

-export([
	start_link/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	code_change/3,
	terminate/2
]).

-export([
	set_timezone/1,
	set_timezone/2,

	get_timezone/0,
	get_timezone/1,

	clear_timezone/0,
	clear_timezone/1

%%	register_parser/1,
%%	register_parser/2,
%%	
%%	deregister_parsers/0,
%%	deregister_parsers/1,
%%
%%	register_format/1,
%%	register_format/2,
%%
%%	deregister_format/0,
%%	deregister_format/1
]).

%% PUBLIC API FUNCTIONS

start_link() ->
	gen_server:start_link({local, ?SRV}, ?MODULE, [], []).

set_timezone(TZ) ->
	set_timezone(self(),TZ).

set_timezone(Key,TZ) ->
	gen_server:call(?SRV,{set_timezone,Key,TZ}).

get_timezone() ->
	get_timezone(self()).

get_timezone(Key) ->
	gen_server:call(?SRV,{get_timezone,Key}).

clear_timezone() ->
	clear_timezone(self()).

clear_timezone(Key) ->
	gen_server:call(?SRV, {clear_timezone, Key}).

%% SERVER FUNCTIONS

-record(state, {tz, parsers, formats}).

init(_) ->
	State = #state{tz=dict:new(),parsers=dict:new(),formats=dict:new()},
	{ok, State}.

handle_cast(_,State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
	erlang:demonitor(MonitorRef),
	NewTZ = dict:erase(Pid, State#state.tz),
	NewParsers = dict:erase(Pid, State#state.parsers),
	NewFormats = dict:erase(Pid, State#state.formats),
	NewState = State#state{tz=NewTZ, parsers=NewParsers, formats=NewFormats},	
	{noreply, NewState };
handle_info(_, State) ->
	{noreply, State}.

handle_call({set_timezone,Key,TZ}, _From, State) ->
	monitor_if_pid(Key),
	NewTZ = dict:store(Key, TZ, State#state.tz),
	NewState = State#state{tz=NewTZ},
	{reply, ok, NewState};
handle_call({clear_timezone,Key},_From, State) ->
	NewTZ = dict:erase(Key, State#state.tz),
	NewState = State#state{tz=NewTZ},
	{reply, ok, NewState};
handle_call({get_timezone,Key},_From, State) ->
	Reply = case dict:find(Key, State#state.tz) of
		error -> undefined;
		{ok, Value} -> Value
	end,
	{reply, Reply, State}.
%% handle_call({register_parser,Key,Parser}) ->
%% handle_call({deregister_parsers,Key}) ->
%% handle_call({register_format,Key,Format}) ->
%% handle_call({deregister_formats,Key}) ->

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% PRIVATE TOOLS

monitor_if_pid(Key) when is_pid(Key) ->
	erlang:monitor(process,Key);
monitor_if_pid(_) ->
	do_nothing.
