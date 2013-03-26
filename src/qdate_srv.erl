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
	clear_timezone/1,

	register_parser/1,
	register_parser/2,
	get_parsers/0,
	deregister_parsers/0,
	deregister_parser/1,

	register_format/2,
	get_format/1,
	deregister_format/1
]).

%% PUBLIC API FUNCTIONS

start_link() ->
	gen_server:start_link({local, ?SRV}, ?MODULE, [], []).

set_timezone(TZ) ->
	set_timezone(self(),TZ).

set_timezone(Key,TZ) ->
	ok = gen_server:call(?SRV,{set_timezone,Key,TZ}).

get_timezone() ->
	get_timezone(self()).

get_timezone(Key) ->
	gen_server:call(?SRV,{get_timezone,Key}).

clear_timezone() ->
	clear_timezone(self()).

clear_timezone(Key) ->
	ok = gen_server:call(?SRV, {clear_timezone, Key}).

register_parser(Parser) when is_function(Parser,1) ->
	register_parser(erlang:make_ref(),Parser).

register_parser(Key,Parser) when is_function(Parser,1) ->
	Key = gen_server:call(?SRV,{register_parser,Key,Parser}).

deregister_parser(Key) ->
	ok = gen_server:call(?SRV,{deregister_parser,Key}).

deregister_parsers() ->
	ok = gen_server:call(?SRV,{deregister_parsers}).

get_parsers() ->
	gen_server:call(?SRV,{get_parsers}).	

register_format(Key,Format) ->
	ok = gen_server:call(?SRV,{register_format,Key,Format}).

get_format(Key) ->
	gen_server:call(?SRV,{get_format,Key}).

deregister_format(Key) ->
	ok = gen_server:call(?SRV,{deregister_format,Key}).
	

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
		{ok,TZ} -> TZ
	end,
	{reply, Reply, State};

handle_call({register_parser,Key,Parser},_From,State) ->
	NewParsers = dict:store(Key, Parser, State#state.parsers),
	NewState = State#state{parsers=NewParsers},
	{reply, Key, NewState};
handle_call({get_parsers},_From,State) ->
	Reply = dict:to_list(State#state.parsers),
	{reply, Reply, State};
handle_call({deregister_parser,Key},_From,State) ->
	NewParsers = dict:erase(Key, State#state.parsers),
	NewState = State#state{parsers=NewParsers},
	{reply, ok, NewState};
handle_call({deregister_parsers},_From,State) ->
	NewState = State#state{parsers=dict:new()},
	{reply, ok, NewState};

handle_call({register_format,Key,Format},_From,State) ->
	NewFormats = dict:store(Key, Format, State#state.formats),
	NewState = State#state{formats=NewFormats},
	{reply, ok, NewState};
handle_call({get_format,Key},_From,State) ->
	Reply = case dict:find(Key, State#state.formats) of
		error -> undefined;
		{ok, Format} -> Format
	end,
	{reply, Reply,State};
handle_call({deregister_format,Key},_From,State) ->
	NewFormats = dict:erase(Key, State#state.formats),
	NewState = State#state{formats=NewFormats},
	{reply, ok, NewState}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% PRIVATE TOOLS

monitor_if_pid(Key) when is_pid(Key) ->
	erlang:monitor(process,Key);
monitor_if_pid(_) ->
	do_nothing.
