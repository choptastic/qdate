-module(qdate_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

        SupFlags = #{},

        ChildSpec = #{
            id=>qdate_srv,
            start=>{qdate_srv, start_link, []}
        },

        {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



