-module(csvlog).

%% API
-export([start/0, log/2]).

start() ->
    {ok, _Started} = application:ensure_all_started(?MODULE).

log(Tag, Values) ->
    case whereis(Tag) of
        undefined ->
            {error, not_found};
        _Pid ->
            gen_server:cast(Tag, {log, Values})
    end.
