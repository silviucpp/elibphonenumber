-module(phonenumber_to_timezones).

-include("libphonenumber.hrl").

-define(TIMEZONES_SEPARATOR, <<"&">>).

-behaviour(gen_server).

-export([
    start_link/0,
    timezones_for_number/1,

    % gen_server

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    timezones_trie
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec timezones_for_number(phonenumber()|binary()) ->
    {ok, [binary()] | false | {error, any()}}.

timezones_for_number(Number) ->
    case elibphone_utils:to_number_object(Number) of
        {ok, Nr} ->
            case phonenumber_util:get_number_type(Nr) of
                unknown ->
                    false;
                _ ->
                    gen_server:call(?MODULE, {timezones_for_number, get_prefix(Nr)})
            end;
        Error ->
            Error
    end.

init([]) ->
    Path = elibphone_utils:get_priv_path(<<"timezones">>),
    {ok, TimezonesMapping} = load_timezones_mapping(<<Path/binary, "/map_data.txt">>),
    {ok, #state{timezones_trie = TimezonesMapping}}.

handle_call({timezones_for_number, Prefix}, _From, #state {timezones_trie = TimezonesTrie} = State) ->
    case btrie:find_prefix_longest(Prefix, TimezonesTrie) of
        {ok, _Prefix, C} ->
            {reply, {ok, C}, State};
        _ ->
            {reply, false, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internals

get_prefix(Number) ->
    case phonenumber_util:is_number_geographical(Number) of
        true ->
            <<(integer_to_binary(phonenumber:get_country_code(Number)))/binary, (phonenumber_util:get_national_significant_number(Number))/binary>>;
        _ ->
            integer_to_binary(phonenumber:get_country_code(Number))
     end.

load_timezones_mapping(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    {ok, btrie:new(get_lines(Device))}.

get_lines(Device) ->
    get_lines(Device, []).

get_lines(Device, Acc) ->
    case io:get_line(Device, "") of
        eof  ->
            file:close(Device),
            Acc;
        Line ->
            LineBin = list_to_binary(Line),

            case LineBin of
                <<"#", _/binary>> ->
                    get_lines(Device, Acc);
                _ ->
                    case binary:split(LineBin, <<"|">>, [global]) of
                        [Prefix, Timezones0] ->
                            Timezones = re:replace(Timezones0, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]),
                            get_lines(Device, [{Prefix, binary:split(Timezones, ?TIMEZONES_SEPARATOR, [global])} | Acc]);
                        _ ->
                            get_lines(Device, Acc)
                    end
            end
    end.
