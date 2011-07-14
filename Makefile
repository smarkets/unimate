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
	rm -rf rel/eto/

dev: dev1 dev2
dev1 dev2:
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

devclean:
	rm -rf dev