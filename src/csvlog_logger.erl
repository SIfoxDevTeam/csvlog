-module(csvlog_logger).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include_lib("kernel/include/file.hrl").

-define(FILE_OPTIONS, [append, raw]).
-define(MAX_SIZE, 10485760). % 10 MB

-define(INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(ERROR(Format, Args), error_logger:error_msg(Format, Args)).

-record(state, {
    file :: string(),
    fd :: file:fd(),
    max_size :: pos_integer(),
    separator :: string(),
    header :: string()
}).

start_link(Tag, Options) ->
    case proplists:get_value(file, Options) of
        undefined ->
            ?ERROR("No CSV file is specified", []);
        File ->
            MaxSize = proplists:get_value(max_size, Options, ?MAX_SIZE),
            Separator = proplists:get_value(separator, Options, ";"),
            Header = proplists:get_value(header, Options, undefined),
            gen_server:start_link({local, Tag}, ?MODULE, [File, MaxSize, Separator, Header], [])
    end.

init([File, MaxSize, Separator, Header]) ->
    {ok, Fd} = open_file(File, Header),
    State = #state{
        file = File,
        fd = Fd,
        max_size = MaxSize,
        separator = Separator,
        header = Header
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Values}, #state{separator = Sep} = State) ->
    Line = prepare_csv_line(Values, Sep),
    case file:read_file_info(State#state.file) of
        {ok, Info} when Info#file_info.size >= State#state.max_size ->
            ok = file:close(State#state.fd),
            rotate_file(State#state.file),
            {ok, FD} = open_file(State#state.file, State#state.header),
            ?INFO("File ~s was successful rotated~n", [State#state.file]),
            ok = file:write(FD, Line ++ "\n"),
            {noreply, State#state{fd = FD}};
        _ ->
            ok = file:write(State#state.fd, Line ++ "\n"),
            {noreply, State}
    end;
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, State) ->
    file:close(State#state.fd).

%% Internal functions
prepare_csv_line(Values, Sep) ->
    L = string:join(convert(Values), Sep),
    string:strip(L, right, $,).

%% In case of tuple we're assume what the record is passed as argument
convert(Values) when is_tuple(Values) ->
    convert(tl(tuple_to_list(Values)), []);
convert(Values) ->
    convert(Values, []).

convert([], Acc) -> lists:reverse(Acc);
convert([H | T], Acc) ->
    convert(T, [cast(H) | Acc]).

-spec cast(term()) -> string().
cast(undefined) -> "";
cast(Value) when is_integer(Value) -> integer_to_list(Value);
cast(Value) when is_atom(Value) -> atom_to_list(Value);
cast(Value) -> Value.

rotate_file(File) ->
    NewName = filename:rootname(File),
    file:rename(File, [NewName, timestamp_suffix(), ".log"]).

localtime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [Year, Month, Day, Hour, Minute, Second]).

timestamp_suffix() ->
    DateTime = lists:flatten(localtime_to_string(erlang:localtime())),
    [Date, Time] = string:tokens(DateTime, " "),
    "-" ++ Date ++ "_" ++ Time.

open_file(File, Header) ->
    case file:open(File, ?FILE_OPTIONS) of
        {ok, Fd} ->
            case file:read_file_info(File) of
                {ok, Info} when Info#file_info.size == 0 andalso Header /= undefined ->
                    file:write(Fd, Header ++ "\n");
                _ -> ok
            end,
            {ok, Fd};
        {error, Reason} ->
            Msg = file:format_error(Reason),
            ?ERROR("Can't open file ~s: ~s~n", [File, Msg]),
            {error, Reason}
    end.
