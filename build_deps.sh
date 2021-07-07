#!/usr/bin/env bash

DEPS_LOCATION=_build/deps
DESTINATION=libphonenumber

if [ -f "$DEPS_LOCATION/$DESTINATION/cpp/build/libphonenumber.a" ]; then
    echo "libphonenumber fork already exist. delete $DEPS_LOCATION/$DESTINATION for a fresh checkout."
    exit 0
fi

LIB_PHONE_NUMBER_ARCHIVE=https://github.com/google/libphonenumber/archive/$1.tar.gz
LIB_PHONE_NUMBER_REPO=https://github.com/googlei18n/libphonenumber.git
LIB_PHONE_NUMBER_REV=$1
DRIVER_SRC="git"
if [ "$2" = "archive" ]; then
  DRIVER_SRC="archive";
  VER=$(echo "$LIB_PHONE_NUMBER_REV" | sed -e 's/^v//g');
fi
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/system-release 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

echo "Use repo ${LIB_PHONE_NUMBER_REPO} and revision ${LIB_PHONE_NUMBER_REV}"
echo "OS detected: ${OS} ${KERNEL}"

function fail_check
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

qmake_unix()
{
	fail_check cmake \
        -DCMAKE_C_FLAGS="-fPIC" \
        -DCMAKE_CXX_FLAGS="-fPIC -std=c++11 " \
        -DCMAKE_INSTALL_PREFIX:PATH=install \
        -DUSE_BOOST=ON \
        -DUSE_RE2=OFF \
        -DUSE_ICU_REGEXP=ON \
        -USE_STDMUTEX=ON \
        -DREGENERATE_METADATA=OFF \
        ..
}

qmake_darwin()
{
    export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig"

	fail_check cmake \
        -DCMAKE_CXX_FLAGS="-std=c++11 " \
        -DCMAKE_INSTALL_PREFIX:PATH=install \
	    -DUSE_BOOST=OFF \
	    -DUSE_RE2=OFF \
	    -DUSE_ICU_REGEXP=ON \
	    -DREGENERATE_METADATA=OFF \
	    -USE_STDMUTEX=ON \
        -DICU_UC_INCLUDE_DIR=/usr/local/opt/icu4c/include \
        -DICU_UC_LIB=/usr/local/opt/icu4c/lib/libicuuc.dylib \
        -DICU_I18N_INCLUDE_DIR=/usr/local/opt/icu4c/include \
        -DICU_I18N_LIB=/usr/local/opt/icu4c/lib/libicui18n.dylib \
        -DGTEST_SOURCE_DIR=../../../googletest/googletest/ \
        -DGTEST_INCLUDE_DIR=../../../googletest/googletest/include/ \
        ..
}

install_libphonenumber()
{
	if [ "${DRIVER_SRC}" = "git" ]; then
		echo "Building from Git tag"
		git clone ${LIB_PHONE_NUMBER_REPO} ${DESTINATION}
		pushd ${DESTINATION}
		fail_check git checkout ${LIB_PHONE_NUMBER_REV}
		popd
	elif [ "${DRIVER_SRC}" = "archive" ]; then
		echo "Building from Git archive"
		wget -q -O libphonenumber.tar.gz $LIB_PHONE_NUMBER_ARCHIVE
		tar -xf libphonenumber.tar.gz
		mv libphonenumber-${VER} libphonenumber
	fi;

	mkdir -p ${DESTINATION}/cpp/build
	pushd ${DESTINATION}/cpp/build

	case $OS in
        Linux)
            qmake_unix
        ;;

        Darwin)
            qmake_darwin
        ;;

        *)
            echo "Your system $OS is not supported"
            exit 1
    esac

	fail_check make -j 8
	fail_check make install
	popd
}

copy_resources()
{
    rm -rf priv
    fail_check mkdir priv
    fail_check cp -R $DEPS_LOCATION/$DESTINATION/resources/carrier priv/carrier
    fail_check cp -R $DEPS_LOCATION/$DESTINATION/resources/timezones priv/timezones
}

run_installation()
{
	mkdir -p $DEPS_LOCATION
	pushd $DEPS_LOCATION

    case $OS in
      Linux)
         case $KERNEL in
            Ubuntu|Debian)
                echo "Check Dependecies for $KERNEL"
                fail_check dpkg -s cmake cmake-curses-gui libgtest-dev libicu-dev protobuf-compiler libprotobuf-dev \
                                   libboost-dev libboost-thread-dev libboost-system-dev
                install_libphonenumber
                ;;
            CentOS|Amazon)
                echo "Check Dependecies for $KERNEL"
                fail_check rpm -q --dump cmake gtest-devel libicu-devel boost-devel protobuf-compiler protobuf-devel
                install_libphonenumber
                ;;
            *)
                echo "Your system $KERNEL is not supported"
         esac
            ;;
      Darwin)
            brew install cmake pkg-config icu4c protobuf

            fail_check git clone https://github.com/google/googletest.git
            pushd googletest
            fail_check git checkout 703bd9caab50b139428cea1aaff9974ebee5742e
            popd

            install_libphonenumber
            pushd ${DESTINATION}/cpp/build
            rm -rf *.dylib
            popd
            ;;
      *)
            echo "Your system $OS is not supported"
            exit 1
    esac

    popd
}

run_installation
copy_resources
