-module(phonenumber_geocoding_test).

-include_lib("eunit/include/eunit.hrl").

get_geocoding_for_number_test() ->
    <<"Mountain View, CA">> = phonenumber_geocoding:get_geocoding_for_number(<<"+16502530000">>),
    <<"Mountain View, CA">> = phonenumber_geocoding:get_geocoding_for_number(phonenumber_util:parse(<<"+16502530000">>, <<"">>)),
    <<"Mountain View, CA">> = phonenumber_geocoding:get_geocoding_for_number(phonenumber_util:parse(<<"+16502530000">>, <<"">>), <<"en">>),
    <<"">> = phonenumber_geocoding:get_geocoding_for_number(phonenumber_util:parse(<<"+39236618300">>, <<"">>), <<"en">>).

