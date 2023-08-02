
PROJECT_NIF_NAME=phonenumber_util_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)

	ICU4_ROOT := $(shell brew --prefix icu4c)
    PROTOBUF_ROOT := $(shell brew --prefix protobuf)

	CXXFLAGS += -I$(PROTOBUF_ROOT)/include \
				-I$(ICU4_ROOT)/include

	LDFLAGS +=  -L$(PROTOBUF_ROOT)/lib \
				-L$(ICU4_ROOT)/lib \
				-lphonenumber \
				-lgeocoding \
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
				-Wl,--start-group
endif

CXXFLAGS +=  -fPIC -g -Wextra -Werror -std=c++17 -fno-exceptions -Wno-unused-parameter -Wno-missing-field-initializers \
			 -I $(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/include

LDFLAGS +=  -L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/lib \
			-labsl_base \
			-labsl_bad_optional_access \
			-labsl_spinlock_wait \
			-labsl_malloc_internal \
			-labsl_throw_delegate \
			-labsl_log_internal_check_op \
			-labsl_log_internal_message \
			-labsl_raw_logging_internal \
			-labsl_hashtablez_sampler \
			-labsl_raw_hash_set \
			-labsl_symbolize \
			-labsl_stacktrace \
			-labsl_debugging_internal \
			-labsl_demangle_internal \
			-labsl_city \
			-labsl_hash \
			-labsl_low_level_hash \
			-labsl_strings \
			-labsl_synchronization \
			-labsl_graphcycles_internal \
			-labsl_time \
			-labsl_time_zone \
			-labsl_int128 \
			-licui18n \
			-licuuc \
			-lprotobuf

