
PROJECT_NIF_NAME=phonenumber_util_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
	CXXFLAGS += -I/usr/local/opt/protobuf/include

	LDFLAGS +=  -L/usr/local/lib/ \
				-L/usr/local/opt/icu4c/lib \
			    -lphonenumber \
				-lboost_thread-mt \
				-lstdc++ \
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
				-Wl,-U,_enif_get_int
else
	LDFLAGS +=  -l:libphonenumber.a \
				-lboost_thread
endif

CXXFLAGS +=  -fPIC -g -Wextra -Werror -std=c++11 -fno-exceptions -Wno-unused-parameter -Wno-missing-field-initializers \
 		     -I $(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/include

LDFLAGS +=  -L$(BASEDIR)/_build/deps/libphonenumber/cpp/build \
			-lboost_system \
			-lboost_date_time \
			-licui18n \
			-licuuc \
			-lprotobuf
