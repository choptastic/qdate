all: get-deps compile

get-deps:
	./rebar3 get-deps

compile:
	./rebar3 compile

test: get-deps compile
	./rebar3 skip_deps=true eunit

run:
	erl -pa ebin/ deps/*/ebin/ -eval "application:start(qdate)"
