-module(phonenumber_to_carrier_test).

-include_lib("eunit/include/eunit.hrl").

carrier_for_number_test() ->
    application:ensure_all_started(elibphonenumber),

    <<"">> = phonenumber_to_carrier:carrier_for_number(<<"19004433030">>, <<"en">>),
    <<"">> = phonenumber_to_carrier:carrier_for_number(<<"18885551234">>, <<"en">>),
    <<"">> = phonenumber_to_carrier:carrier_for_number(<<"+393123456789">>, <<"en">>),
    <<"">> = phonenumber_to_carrier:carrier_for_number(<<"+12423651234">>, <<"en">>),

    <<"Vodafone">> = phonenumber_to_carrier:carrier_for_number(<<"447436555511">>, <<"en">>),
    <<>> = phonenumber_to_carrier:carrier_for_number(<<"AAAA">>, <<"en">>).
