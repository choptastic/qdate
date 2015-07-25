all: compile

compile:
	./rebar3 compile

test: compile
	./rebar3 eunit

run:
	erl -pa ebin/ deps/*/ebin/ -eval "application:start(qdate)"
