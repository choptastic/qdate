REBAR = $(shell pwd)/rebar3

all: compile

compile:
	$(REBAR) compile

test: compile
	$(REBAR) eunit

run:
	erl -pa ebin/ deps/*/ebin/ -eval "application:start(qdate)"

publish:
	$(REBAR) as pkg upgrade
	$(REBAR) as pkg hex publish
	$(REBAR) upgrade

