% vim: ts=4 sw=4 et
% Copyright (c) 2013 Jesse Gumm
% See LICENSE for licensing information.

-module(qdate_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    qdate_sup:start_link().

stop(_State) ->
    ok.
