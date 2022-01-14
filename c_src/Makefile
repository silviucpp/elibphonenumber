
PROJECT_NIF_NAME=phonenumber_util_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
	CXXFLAGS += -I/usr/local/opt/protobuf/include \
				-I/usr/local/opt/icu4c/include

	LDFLAGS +=  -L/usr/local/lib/ \
				-L/usr/local/opt/icu4c/lib \
				-lphonenumber \
				-lgeocoding \
				-lboost_thread-mt \
				-lstdc++ \
				-framework CoreFoundation \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_tuple \
				-Wl,-U,_enif_make_ulong \
				-Wl,-U,_enif_make_list_from_array \
				-Wl,-U,_enif_make_int \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_badarg \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_inspect_binary\
				-Wl,-U,_enif_get_ulong \
				-Wl,-U,_enif_get_uint64 \
				-Wl,-U,_enif_get_tuple \
				-Wl,-U,_enif_get_int \
				-Wl,-U,_enif_alloc \
				-Wl,-U,_enif_free \
				-Wl,-U,_enif_priv_data
else
	LDFLAGS +=  -l:libphonenumber.a \
				-l:libgeocoding.a \
				-lboost_thread
endif

CXXFLAGS +=  -fPIC -g -Wextra -Werror -std=c++11 -fno-exceptions -Wno-unused-parameter -Wno-missing-field-initializers \
			 -I $(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/include \
			 -I $(BASEDIR)/_build/deps/libphonenumber/cpp/build/_deps/abseil-cpp-src

LDFLAGS +=  -L$(BASEDIR)/_build/deps/libphonenumber/cpp/build \
            -L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/_deps/abseil-cpp-build/absl/base \
			-L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/_deps/abseil-cpp-build/absl/debugging \
			-L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/_deps/abseil-cpp-build/absl/strings \
			-L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/_deps/abseil-cpp-build/absl/synchronization \
			-L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/_deps/abseil-cpp-build/absl/time \
			-L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/_deps/abseil-cpp-build/absl/numeric \
			-lboost_system \
			-lboost_date_time \
			-licui18n \
			-licuuc \
			-lprotobuf \
			-Wl,--start-group \
			-labsl_base \
			-labsl_malloc_internal \
			-labsl_throw_delegate \
			-labsl_raw_logging_internal \
			-labsl_stacktrace \
			-labsl_debugging_internal \
			-labsl_demangle_internal \
			-labsl_strings \
			-labsl_synchronization \
			-labsl_graphcycles_internal \
			-labsl_time \
			-labsl_time_zone \
			-labsl_int128 \
			-labsl_spinlock_wait \
			-labsl_symbolize
