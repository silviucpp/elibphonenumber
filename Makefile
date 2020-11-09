.DEFAULT_GOAL := compile

REBAR=rebar3

C_SRC_DIR = $(shell pwd)/c_src
C_SRC_ENV ?= $(C_SRC_DIR)/env.mk

#regenerate all the time the env.mk
ifneq ($(wildcard $(C_SRC_DIR)),)
	GEN_ENV ?= $(shell erl -noshell -s init stop -eval "file:write_file(\"$(C_SRC_ENV)\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		halt().")
    $(GEN_ENV)
endif

include $(C_SRC_ENV)

nif_compile:
	@./build_deps.sh $(DRIVER_REV) $(DRIVER_SRC)
	@make V=0 -C c_src -j 8

nif_clean:
	@make -C c_src clean

compile:
	${REBAR} compile

clean:
	${REBAR} clean

cpplint:
	cpplint --counting=detailed \
			--filter=-legal/copyright,-build/include_subdir,-build/include_order,-whitespace/braces,-whitespace/parens,-whitespace/newline \
			--linelength=300 \
			--exclude=c_src/*.o --exclude=c_src/*.mk  \
			c_src/*.*

cppcheck:
	cppcheck --enable=all \
	 		 -I _build/deps/libphonenumber/cpp/build/install/include \
	 		 -I /usr/local/include \
	 		 -I $(ERTS_INCLUDE_DIR) \
	 		 -I $(ERL_INTERFACE_INCLUDE_DIR) \
	 		 --xml-version=2 \
	 		 --output-file=cppcheck_results.xml \
	 		 c_src/
