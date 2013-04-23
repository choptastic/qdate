all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

test: compile
	./rebar skip_deps=true eunit

run:
	erl -pa ebin/ deps/*/ebin/
