
PROJECT_NIF_NAME=phonenumber_util_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)

	ICU4_ROOT := $(shell brew --prefix icu4c)
    PROTOBUF_ROOT := $(shell brew --prefix protobuf@3)
    ABSEIL_ROOT := $(shell brew --prefix abseil 2>/dev/null)

    CXXFLAGS += -I$(PROTOBUF_ROOT)/include \
                -I$(ICU4_ROOT)/include

    LDFLAGS += -L$(PROTOBUF_ROOT)/lib \
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
               -Wl,-U,_enif_inspect_binary \
               -Wl,-U,_enif_get_ulong \
               -Wl,-U,_enif_get_uint64 \
               -Wl,-U,_enif_get_tuple \
               -Wl,-U,_enif_get_int \
               -Wl,-U,_enif_alloc \
               -Wl,-U,_enif_free \
               -Wl,-U,_enif_priv_data
else
    LDFLAGS += -l:libphonenumber.a \
               -l:libgeocoding.a \
               -Wl,--start-group
endif

CXXFLAGS += -fPIC -g -Wextra -Werror -std=c++17 -fno-exceptions \
            -Wno-unused-parameter -Wno-missing-field-initializers \
            -I $(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/include

LDFLAGS += -L$(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/lib

ifeq ($(wildcard $(ABSEIL_ROOT)/lib/libabsl_*.dylib),)
    # Brew Abseil not found -> use local
    ABSL_LIB_DIR := $(BASEDIR)/_build/deps/libphonenumber/cpp/build/install/lib
    ABSL_LIBS := $(wildcard $(ABSL_LIB_DIR)/libabsl_*.a)
    LDFLAGS += -L$(ABSL_LIB_DIR)
    LDFLAGS += $(patsubst $(ABSL_LIB_DIR)/lib%.a,-l%, $(ABSL_LIBS))
else
    # Brew Abseil found -> use brew
    ABSL_LIB_DIR := $(ABSEIL_ROOT)/lib
    ABSL_LIBS := $(wildcard $(ABSL_LIB_DIR)/libabsl_*.dylib)
    LDFLAGS += -L$(ABSL_LIB_DIR)
    LDFLAGS += $(patsubst $(ABSL_LIB_DIR)/lib%.dylib,-l%, $(ABSL_LIBS))
endif

ifneq ($(UNAME_SYS), darwin)
    LDFLAGS += -Wl,--end-group
endif

LDFLAGS += -licui18n -licuuc -lprotobuf
