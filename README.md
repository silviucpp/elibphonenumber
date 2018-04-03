elibphonenumber
===============

Erlang port of [libphonenumber][1]

##### Note

1. `libphonenumber` version is given in makefile

##### Ubuntu

On the latest versions it's enough to do:

```bash
sudo apt-get install cmake cmake-curses-gui libgtest-dev libre2-dev libicu-dev 
sudo apt-get install libboost-dev libboost-thread-dev libboost-system-dev
sudo apt-get install libprotobuf-dev protobuf-compiler pkg-config
```

##### Mac Os

On `Mac OS` make sure you have `brew` installed and rebar will automatically install all necessary dependencies.
 
## Get carrier for number
    
In order to do this make sure the application is started then use `phonenumber_to_carrier:carrier_for_number/2` method    
    
```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_to_carrier:carrier_for_number(<<"44743655551">>, <<"en">>).
```
