all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

test: get-deps compile
	./rebar skip_deps=true eunit

run:
	erl -pa ebin/ deps/*/ebin/ -eval "application:start(qdate)"
