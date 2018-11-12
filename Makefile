.DEFAULT_GOAL := compile

REBAR=rebar

nif_compile:
	@./build_deps.sh $(DRIVER_REV)
	@make V=0 -C c_src -j 8

nif_clean:
	rm -rf deps
	@make -C c_src clean

compile:
	${REBAR} compile

clean:
	${REBAR} clean

