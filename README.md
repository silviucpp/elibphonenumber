# elibphonenumber

[![Build Status](https://app.travis-ci.com/silviucpp/elibphonenumber.svg?branch=master)](https://travis-ci.com/github/silviucpp/elibphonenumber)
[![GitHub](https://img.shields.io/github/license/silviucpp/elibphonenumber)](https://github.com/silviucpp/elibphonenumber/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/elibphonenumber)](https://hex.pm/packages/elibphonenumber)

Erlang driver for [libphonenumber][1].

## Notes

- This project is actively maintained, with frequent updates due to the original author's inactivity.
- Compatible with both `rebar` and `rebar3`, as well as `hex`.
- To change the `libphonenumber` version, modify the `DRIVER_REV` argument in `rebar.config`.
- By default, the build uses a git tag archive rather than a full clone and tag checkout. You can change this behavior by modifying the `DRIVER_SRC` argument in `rebar.config`.

## Compilation

Ensure all dependencies needed to build `libphonenumber` are installed. Below is a summary for each operating system where the library has been tested. If you encounter issues, consult the [libphonenumber documentation][2] for additional guidance.

### Ubuntu or Debian

For the latest versions, run:

```bash
sudo apt-get install cmake cmake-curses-gui libicu-dev 
sudo apt-get install libprotobuf-dev protobuf-compiler
```

### CentOS 7

Enable [EPEL][3] (required for RE2):

```bash
sudo yum install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
```

Then install the required packages:

```bash
sudo yum install cmake git libicu-devel protobuf-devel protobuf-compiler
```

### macOS

Make sure you have `brew` installed. Rebar will automatically install all necessary dependencies.

### Speeding Up the Compilation Process

You can use `ccache` to speed up the compilation:

- Install `ccache` on your platform (e.g., `sudo apt-get install ccache` for Debian-based systems or `brew install ccache` for macOS).
- In `rebar.config.script`, add:

```erlang
os:putenv("ELIBPHONENUMBER_USE_CCACHE", "1"),
Config.
```

## Get Carrier for a Number

Ensure the application is started, then use the `phonenumber_to_carrier:carrier_for_number/2` method:

```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_to_carrier:carrier_for_number(<<"44743655551">>, <<"en">>).
```

## Get Timezones for a Number

To retrieve the timezones associated with a number, call `phonenumber_to_timezones:timezones_for_number/1` as follows:

```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_to_timezones:timezones_for_number(<<"16502530000">>).
{ok,[<<"America/Los_Angeles">>]}
```

## Geocoding

To get the geographical area of a phone number (if available), use the following method:

```erlang 
application:ensure_all_started(elibphonenumber).
phonenumber_geocoding:get_geocoding_for_number(<<"+16502530000">>),
<<"Mountain View, CA">>
```

## Run the Tests

```bash
rebar3 eunit
```

[1]: https://github.com/googlei18n/libphonenumber
[2]: https://github.com/googlei18n/libphonenumber/blob/master/cpp/README
[3]: https://fedoraproject.org/wiki/EPEL#Quickstart
