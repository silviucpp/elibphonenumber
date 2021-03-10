-module(elibphone_utils).

-include("libphonenumber.hrl").

-define(PLUS_SIGN_DIGIT, 43).

-export([
    get_priv_path/1,
    to_number_object/1,
    trim_plus_sign/1,
    append_plus_sign/1
]).

get_priv_path(File) ->
    case code:priv_dir(elibphonenumber) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.

to_number_object(#phonenumber{} = N) ->
    {ok, N};
to_number_object(NumberBin) when is_binary(NumberBin) ->
    case phonenumber_util:parse(append_plus_sign(NumberBin), <<"ZZ">>) of
        {error, _} = R ->
            R;
        Number ->
            {ok, Number}
    end.

trim_plus_sign(P) when is_binary(P) ->
    case binary:first(P) of
        ?PLUS_SIGN_DIGIT ->
            <<_:1/binary, Value/binary>> = P,
            Value;
        _ ->
            P
    end.

append_plus_sign(P) when is_binary(P) ->
    case binary:first(P) of
        ?PLUS_SIGN_DIGIT ->
            P;
        _ ->
            <<"+", P/binary>>
    end.
