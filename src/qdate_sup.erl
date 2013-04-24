% vim: ts=4 sw=4 et
% Copyright (c) 2013 Jesse Gumm
% See LICENSE for licensing information.

-module(qdate_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server = ?CHILD(qdate_srv, worker),
    {ok, { {one_for_one, 5, 10}, [Server]} }.

