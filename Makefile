DEPS_DIR=deps
DEPS=${DEPS_DIR}/exmpp

.PHONY: rel dev debug

all: compile

compile: ${DEPS}
	./rebar compile

clean:
	./rebar clean

depsclean:
	./rebar delete-deps get-deps

depsupdate:
	./rebar update-deps

${DEPS}: rebar.config
	./rebar get-deps

test:
	./rebar skip_deps=true eunit

rel: ${DEPS}
	./rebar compile generate

relclean:
	rm -rf rel/unimate

live: compile
	mkdir -p live
	(cd rel && ../rebar generate target_dir=../live/$@ overlay_vars=vars/$@.config)

liveclean:
	rm -rf live

dev: compile
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

devclean:
	rm -rf dev
