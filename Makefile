.PHONY: rel deps dev debug

all: compile

compile:
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps

depsclean:
	./rebar delete-deps get-deps

depsupdate:
	./rebar update-deps

test:
	./rebar skip_deps=true eunit

rel: deps
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
