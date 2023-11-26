-module(phonenumber_to_carrier).

-include("libphonenumber.hrl").
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
    start_link/0,
    carrier_for_number/2,

    % gen_server

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    carrier_maps
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec carrier_for_number(phonenumber()|binary(), binary()) ->
    binary().

carrier_for_number(Number, Lang) ->
    case elibphone_utils:to_number_object(Number) of
        {ok, Nr} ->
            case is_mobile(phonenumber_util:get_number_type(Nr)) of
                true ->
                    E164NoSign = elibphone_utils:trim_plus_sign(phonenumber_util:format(Nr, e164)),
                    gen_server:call(?MODULE, {carrier_for_number, E164NoSign, Lang});
                _ ->
                    <<>>
            end;
        _Error ->
            <<>>
    end.

init([]) ->
    Path = elibphone_utils:get_priv_path(<<"carrier">>),
    {ok, AllLanguages0} = file:list_dir(Path),

    AllLanguages = lists:map(fun(X) -> list_to_binary(X) end, AllLanguages0),
    AdditionalPathsMap = elibphone_utils:get_env(additional_carriers_mapping, maps:new()),

    FunLang = fun(Lang, Acc) ->
        LangPath = <<Path/binary, "/", Lang/binary>>,
        AdditionalMappingFiles = maps:get(Lang, AdditionalPathsMap, []),

        case load_carrier_mapping([LangPath|AdditionalMappingFiles]) of
            {ok, Trie} ->
                maps:put(Lang, Trie, Acc);
            _ ->
                Acc
        end
    end,

    {ok, #state{carrier_maps = lists:foldl(FunLang, maps:new(), AllLanguages)}}.

handle_call({carrier_for_number, Number, Lang}, _From, #state {carrier_maps = Maps} = State) ->
    Carrier = case maps:find(Lang, Maps) of
        {ok, Trie} ->
            case btrie:find_prefix_longest(Number, Trie) of
                {ok, _Prefix, C} ->
                    C;
                _ ->
                    <<>>
            end;
        _ ->
            <<>>
    end,
    {reply, Carrier, State};

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

% internals

is_mobile(mobile) ->
    true;
is_mobile(fixed_line_or_mobile) ->
    true;
is_mobile(pager) ->
    true;
is_mobile(_) ->
    false.

load_carrier_mapping(Path) ->
    case accumulate_files(Path, []) of
        {ok, AllFiles} ->
            FunFile = fun(FilePath, Acc) ->
                case binary:match(FilePath, <<".txt">>,[]) of
                    nomatch ->
                        Acc;
                    _ ->
                        {ok, Device} = file:open(FilePath, [read]),
                        get_lines(Device) ++ Acc
                end
            end,

            AllMappings = lists:foldl(FunFile, [], AllFiles),
            {ok, btrie:new(AllMappings)};
        _ ->
            false
    end.

accumulate_files([H|T], Acc) ->
    case filelib:is_dir(H) of
        true ->
            case file:list_dir(H) of
                {ok, Files0} ->
                    Files = lists:map(fun(P) -> <<H/binary, "/", (list_to_binary(P))/binary>> end, Files0),
                    accumulate_files(T, Files++Acc);
                Error ->
                    ?LOG_ERROR("~p : failed to load files from: ~p with error: ~p", [?MODULE, H, Error]),
                    Error
            end;
        _ ->
            accumulate_files(T, [H| Acc])
    end;
accumulate_files([], Acc) ->
    {ok, Acc}.

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
                        [Prefix, Carrier0] ->
                            Carrier = re:replace(Carrier0, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]),
                            get_lines(Device, [{Prefix, Carrier}| Acc]);
                        _ ->
                            get_lines(Device, Acc)
                    end
            end
    end.
