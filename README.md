elibphonenumber
===============
[![Build Status](https://app.travis-ci.com/silviucpp/elibphonenumber.svg?branch=master)](https://travis-ci.com//github/silviucpp/elibphonenumber)
[![GitHub](https://img.shields.io/github/license/silviucpp/elibphonenumber)](https://github.com/silviucpp/elibphonenumber/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/elibphonenumber)](https://hex.pm/packages/elibphonenumber)

Erlang driver for [libphonenumber][1].

## Notes

- This project is active. I'm doing updates very often because I see the original author is no longer maintaining it.
- Compatible with both `rebar` and `rebar3` or `hex`.
- To change the `libphonenumber` version modify in `rebar.config` the `DRIVER_REV` argument sent to `make`.
- By default, the build is using git tag archive rather than full clone and tag checkout. You can change this behaviour by modifying in `rebar.config` the `DRIVER_SRC` argument sent to `git`. 

## Compile

In order to compile you need to make sure all dependencies needed to build `libphonenumber` are already installed.

Next you can find a resume for each operating system where library was tested but in case you encounter problems you can
consult as well the documentation from building `libphonenumber` located [here][2]

##### Ubuntu or Debian

On the latest versions it's enough to do:

```bash
sudo apt-get install cmake cmake-curses-gui libicu-dev 
sudo apt-get install libprotobuf-dev protobuf-compiler
```

##### CentOS 7

Enable [EPEL][3] (for RE2):
```bash
sudo yum install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
```

And install packages:
```bash
sudo yum install cmake git libicu-devel protobuf-devel protobuf-compiler
```

##### Mac Os

On `Mac OS` make sure you have `brew` installed and rebar will automatically install all necessary dependencies.

## Get carrier for number

In order to do this make sure the application is started then use `phonenumber_to_carrier:carrier_for_number/2` method

```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_to_carrier:carrier_for_number(<<"44743655551">>, <<"en">>).
```

## Get Timezones for a number

In order to get the timezones associated with a number you can call `phonenumber_to_timezones:timezones_for_number/1` method as follow:

```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_to_timezones:timezones_for_number(<<"16502530000">>).
{ok,[<<"America/Los_Angeles">>]}
```

## Geocoding

Using the following method you can get the geographical area of a phone number (in case this information is available).

```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_geocoding:get_geocoding_for_number(<<"+16502530000">>),
<<"Mountain View, CA">>
```

## Run the tests

```bash
rebar3 eunit
```

[1]: https://github.com/googlei18n/libphonenumber
[2]: https://github.com/googlei18n/libphonenumber/blob/master/cpp/README
[3]: https://fedoraproject.org/wiki/EPEL#Quickstart
