-module(csvlog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, csvlog_logger).

-define(SPEC(Tag, Opts),
    {Tag, {?SERVER, start_link, [Tag, Opts]}, permanent, 5000, worker, [?SERVER]}
).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, specs()}}.

%% Internal functions
specs() ->
    [?SPEC(Tag, Opts) || {Tag, Opts} <- application:get_env(csvlog, destinations, [])].
