-module(qdate_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Copy app env to persistent_terms
    [persistent_term:put({qdate, Key}, Value)
     || {Key, Value} <- application:get_all_env(qdate)],
    qdate_sup:start_link().


stop(_State) ->
    ok.


