elibphonenumber
===============

Erlang port of [libphonenumber](https://github.com/googlei18n/libphonenumber).

##Note

- This project is active. I'm doing updates very offen because I see the original author is no longer maintaining it.
- This repository is a fork of [silviucpp/elibphonenumber][3], except this version is designed to be used with rebar3.

## Compile

You need compile `libphonenumber` and install it before compiling the project.

On ubuntu, before compiling libphonenumber, you need the following dependencies:

```sh
# Works on Ubuntu Vivid and up
sudo apt-get install -y cmake cmake-curses-gui libprotobuf-dev \
                        libgtest-dev libre2-dev libicu-dev \
                        libboost-dev libboost-thread-dev \
                        libboost-system-dev protobuf-compiler \
                        git
```

You can run the following to build a `.deb` for ubuntu:

```bash
#!/usr/bin/env bash

LIB_PHONE_NUMBER_REPO=https://github.com/googlei18n/libphonenumber.git
LIB_PHONE_NUMBER_REV=master

function log()
{
  echo " -> $1"
}

log "Checking out Repo"
git clone --depth=1 -b ${LIB_PHONE_NUMBER_REV} ${LIB_PHONE_NUMBER_REPO}

mkdir -p libphonenumber/cpp/build
cd libphonenumber/cpp/build

export CFLAGS=-fPIC
export CXXFLAGS=-fPIC

log "Adding Debian packaging information to CMakeLists.txt"

echo 'set(CPACK_GENERATOR "DEB")' >> ../CMakeLists.txt
echo 'set(CPACK_DEBIAN_PACKAGE_MAINTAINER "John Hamelink")' >> ../CMakeLists.txt
echo 'set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS true)' >> ../CMakeLists.txt
echo "set(CPACK_DEBIAN_PACKAGE_VERSION \"\${libphonenumber_VERSION_MAJOR}.\${libphonenumber_VERSION_MINOR}.${BUILD_NUMBER}\")" >> ../CMakeLists.txt
echo 'set(CPACK_PACKAGE_FILE_NAME "libphonenumber-${CPACK_DEBIAN_PACKAGE_VERSION}-Linux")' >> ../CMakeLists.txt
echo "include(CPack)" >> ../CMakeLists.txt

log "Producing Makefile"
cmake ..

log "Building Package"
make package
```

## Run the tests

```sh
rebar3 compile
rebar3 eunit
```

[1]:https://www.wowapp.com/w/silviu/Silviu-Caragea
[3]:https://github.com/silviucpp/elibphonenumber
