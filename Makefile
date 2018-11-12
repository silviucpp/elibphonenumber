all: build-so build-ebin

distclean:
	rm -rf deps
	@make -C c_src clean
	@rebar clean

build-so:
	@./build_deps.sh v8.9.16
	@make V=0 -C c_src -j 8

build-ebin:
	@rebar get-deps
	@rebar compile

tests:
	@rebar eunit

.PHONY: distclean build-so build-ebin tests compile clean
