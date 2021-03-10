-module(phonenumber_geocoding).

-include("libphonenumber.hrl").

-export([
    get_geocoding_for_number/1,
    get_geocoding_for_number/2
]).

%% @doc Returns a text description for the given phone number, in the default language.
%% The description might consist of the name of the country where
%% the phone number is from, or the name of the geographical area the phone
%% number is from if more detailed information is available. Returns an empty
%% string if the number could come from multiple countries, or the country
%% code is in fact invalid.
%%
%% This method assumes the validity of the number passed in has already been
%% checked, and that the number is suitable for geocoding. We consider
%% fixed-line and mobile numbers possible candidates for geocoding.

-spec get_geocoding_for_number(Number::phonenumber()|binary()) ->
    binary().

get_geocoding_for_number(Number) ->
    get_geocoding_for_number(Number, <<>>).

%% @doc Returns a text description for the given phone number, in the language
%% provided. The description might consist of the name of the country where
%% the phone number is from, or the name of the geographical area the phone
%% number is from if more detailed information is available. Returns an empty
%% string if the number could come from multiple countries, or the country
%% code is in fact invalid.
%%
%% This method assumes the validity of the number passed in has already been
%% checked, and that the number is suitable for geocoding. We consider
%% fixed-line and mobile numbers possible candidates for geocoding.

-spec get_geocoding_for_number(Number::phonenumber()|binary(), Locale::binary()) ->
    binary().

get_geocoding_for_number(Number, Locale) ->
    case elibphone_utils:to_number_object(Number) of
        {ok, Nr} ->
            phonenumber_util:get_geocoding_for_number(Nr, Locale);
        _Error ->
            <<>>
    end.
