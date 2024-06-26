#!/usr/bin/env bash

DEPS_LOCATION=_build/deps
DESTINATION=libphonenumber

if [ -f "$DEPS_LOCATION/$DESTINATION/cpp/build/libphonenumber.a" ]; then
    echo "libphonenumber fork already exist. delete $DEPS_LOCATION/$DESTINATION for a fresh checkout."
    exit 0
fi

LIB_PHONE_NUMBER_ARCHIVE=https://github.com/google/libphonenumber/archive/refs/tags/$1.tar.gz
LIB_PHONE_NUMBER_REPO=https://github.com/googlei18n/libphonenumber.git
LIB_PHONE_NUMBER_REV=$1

DRIVER_SRC="git"
if [ "$2" = "archive" ]; then
  DRIVER_SRC="archive";
  ARCHIVE_VER=$(echo "$LIB_PHONE_NUMBER_REV" | sed -e 's/^v//g');
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
        -DCMAKE_CXX_FLAGS="-fPIC -std=c++11" \
        -DCMAKE_INSTALL_PREFIX:PATH=install \
        -DBUILD_TESTING=OFF \
        -DBUILD_SHARED_LIBS=OFF \
        -DUSE_BOOST=OFF \
        -USE_STDMUTEX=ON \
        ..
}

qmake_darwin()
{
    ICU4_DIR=$(brew --prefix icu4c)
    PROTOBUF_DIR=$(brew --prefix protobuf@3)

    export PKG_CONFIG_PATH="$ICU4_DIR/lib/pkgconfig"

	fail_check cmake \
      -DCMAKE_CXX_FLAGS="-std=c++11 -DNDEBUG" \
      -DCMAKE_INSTALL_PREFIX:PATH=install \
      -DBUILD_TESTING=OFF \
      -DBUILD_SHARED_LIBS=OFF \
      -DUSE_BOOST=OFF \
      -USE_STDMUTEX=ON \
      -DICU_UC_INCLUDE_DIR=$ICU4_DIR/include \
      -DICU_UC_LIB=$ICU4_DIR/lib/libicuuc.dylib \
      -DICU_I18N_INCLUDE_DIR=$ICU4_DIR/include \
      -DICU_I18N_LIB=$ICU4_DIR/lib/libicui18n.dylib \
      -DPROTOBUF_INCLUDE_DIR=$PROTOBUF_DIR/include \
      -DPROTOBUF_LIB=$PROTOBUF_DIR/lib \
      -DPROTOC_BIN=$PROTOBUF_DIR/bin/protoc \
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
		fail_check wget -q -O libphonenumber.tar.gz $LIB_PHONE_NUMBER_ARCHIVE
		fail_check tar -xf libphonenumber.tar.gz
		fail_check rm libphonenumber.tar.gz
		fail_check mv libphonenumber-${ARCHIVE_VER} ${DESTINATION}
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
                fail_check dpkg -s cmake cmake-curses-gui libicu-dev protobuf-compiler libprotobuf-dev
                install_libphonenumber
                ;;
            CentOS|Amazon)
                echo "Check Dependecies for $KERNEL"
                fail_check rpm -q --dump cmake libicu-devel protobuf-compiler protobuf-devel
                install_libphonenumber
                ;;
            *)
                echo "Your system $KERNEL is not supported"
         esac
            ;;
      Darwin)
            export HOMEBREW_NO_INSTALL_UPGRADE=true
            export HOMEBREW_NO_INSTALL_CLEANUP=true
            export HOMEBREW_NO_AUTO_UPDATE=1
            brew install cmake pkg-config icu4c protobuf@3 wget
            install_libphonenumber
            ;;
      *)
            echo "Your system $OS is not supported"
            exit 1
    esac

    popd
}

run_installation
copy_resources
