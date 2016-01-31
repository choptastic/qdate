REBAR = $(shell pwd)/rebar3

all: compile

compile:
	$(REBAR) compile

update:
	$(REBAR) update

test: compile
	$(REBAR) eunit

run:
	$(REBAR) shell

publish:
	$(REBAR) as pkg upgrade
	$(REBAR) as pkg hex publish
	$(REBAR) upgrade

